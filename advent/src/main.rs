use anyhow::{anyhow, bail, Context as _, Result};
use clap::{Parser, Subcommand};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io::{BufRead as _, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

#[derive(Parser)]
struct CliOptions {
    #[command(subcommand)]
    command: CliCommand,
}

#[derive(Subcommand)]
enum CliCommand {
    /// Create new crate to solve a day's question
    NewDay(NewDayOptions),
    /// Submit the answer to AoC servers
    Submit(SubmitOptions),
    /// Initial set-up of the project, must be run from the root
    Init(InitOptions),
}

#[derive(Parser)]
struct NewDayOptions {
    /// The directory and crate name for the new day
    #[arg(long)]
    crate_name: Option<String>,
    /// The day number to use for the new day
    #[arg(long)]
    day: Option<u32>,
}

#[derive(Serialize, Deserialize)]
struct DayMetadata {
    day: u32,
    part_1_complete: bool,
    part_2_complete: bool,
}

impl DayMetadata {
    fn next_part(&self) -> Result<u32> {
        if !self.part_1_complete {
            Ok(1)
        } else if !self.part_2_complete {
            Ok(2)
        } else {
            Err(anyhow!("all parts submitted for day"))
        }
    }
}

const AOC_METADATA_TOML: &str = "aoc-metadata.toml";

impl DayMetadata {
    fn read(day_path: &Path) -> Result<Self> {
        Ok(toml::de::from_str(&fs::read_to_string(
            day_path.join(AOC_METADATA_TOML),
        )?)?)
    }

    fn write(&self, day_path: &Path) -> Result<()> {
        fs::write(
            day_path.join(AOC_METADATA_TOML),
            toml::ser::to_string_pretty(self)?,
        )?;
        Ok(())
    }
}

#[derive(Parser)]
struct InitOptions {
    #[arg(long)]
    year: u32,
}

#[derive(Parser)]
struct SubmitOptions {
    /// The crate to run
    #[arg(long)]
    crate_name: Option<String>,

    /// The day with to submit an answer for
    #[arg(long)]
    day: Option<u32>,

    /// The part to submit an answer for
    #[arg(long)]
    part: Option<u32>,

    /// Don't actually submit the answer
    #[arg(long, action)]
    dry_run: bool,
}

fn num_to_word(n: u32) -> Option<&'static str> {
    const WORDS: [&str; 25] = [
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
        "ten",
        "eleven",
        "twelve",
        "thirteen",
        "fourteen",
        "fifteen",
        "sixteen",
        "seventeen",
        "eighteen",
        "nineteen",
        "twenty",
        "twenty-one",
        "twenty-two",
        "twenty-three",
        "twenty-four",
        "twenty-five",
    ];
    WORDS.get(n as usize - 1).copied()
}

const CARGO_TOML: &str = r#"
[package]
name = "<name>"
version = "0.1.0"
edition = "2021"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
advent.workspace = true
parse.workspace = true
combine.workspace = true
"#;

const MAIN_RS: &str = r#"
use advent::prelude::*;

#[part_one]
fn part_one(_: String) -> &'static str {
    "incomplete"
}

#[part_two]
fn part_two(_: String) -> &'static str {
    "incomplete"
}

harness!();

"#;

const AOC_YEAR: &str = "aoc-year";

fn aoc_year(root: &Path) -> Result<u32> {
    Ok(fs::read_to_string(root.join(AOC_YEAR))
        .with_context(|| format!("year missing: ./{AOC_YEAR}"))?
        .trim()
        .parse()?)
}

fn aoc_token() -> Result<String> {
    Ok(fs::read_to_string(
        homedir::my_home()?
            .ok_or_else(|| anyhow!("home directory set"))?
            .join(".config/aoc/token"),
    )
    .with_context(|| "session key missing: ~/.config/aoc/token")?
    .trim()
    .into())
}

fn download_input(root: &Path, crate_name: &str, day: u32) -> Result<()> {
    let year = aoc_year(root)?;
    let client = reqwest::blocking::Client::new();
    let resp = client
        .get(format!("https://adventofcode.com/{year}/day/{day}/input"))
        .header("cookie", format!("session={}", aoc_token()?))
        .send()?;

    let input_text = resp.text()?;
    if input_text == "Puzzle inputs differ by user.  Please log in to get your puzzle input." {
        bail!("session key invalid");
    }

    fs::write(root.join(crate_name).join("input.txt"), input_text)?;
    Ok(())
}

fn add_to_workspace(root: &Path, crate_name: &str) -> Result<()> {
    let toml = fs::read_to_string(root.join("Cargo.toml"))?;
    let mut cargo_toml: toml::Value = toml::from_str(&toml)?;
    let members = cargo_toml["workspace"]["members"]
        .as_array_mut()
        .ok_or_else(|| anyhow!("workspace.members present"))?;
    members.push(crate_name.into());
    members.sort_by_key(|v| v.as_str().map(|s| s.to_string()).unwrap_or_default());

    fs::write(
        root.join("Cargo.toml"),
        toml::to_string_pretty(&cargo_toml)?,
    )?;

    Ok(())
}

fn get_existing_days(root: &Path) -> Result<BTreeMap<u32, (PathBuf, DayMetadata)>> {
    let mut days = BTreeMap::new();
    for entry in fs::read_dir(root)? {
        let path = entry?.path();
        if path.join(AOC_METADATA_TOML).exists() {
            let metadata = DayMetadata::read(&path)?;
            days.insert(metadata.day, (path, metadata));
        }
    }
    Ok(days)
}

fn find_next_day(root: &Path) -> Result<u32> {
    let days = get_existing_days(root)?;
    if let Some((d, _)) = days.last_key_value() {
        Ok(*d + 1)
    } else {
        Ok(1)
    }
}

fn resolve_day(root: &Path, day: Option<u32>) -> Result<u32> {
    if let Some(day) = day {
        Ok(day)
    } else {
        Ok(find_next_day(root)?)
    }
}

fn given_crate_name_or_guessed_from_day(day: u32, crate_name: &Option<String>) -> Result<String> {
    if let Some(crate_name) = crate_name {
        Ok(crate_name.to_owned())
    } else {
        Ok(num_to_word(day)
            .ok_or_else(|| anyhow!("couldn't determine crate name for day {day}"))?
            .into())
    }
}

fn add_new_day(crate_name: &Option<String>, day: Option<u32>) -> Result<()> {
    let root = find_aoc_root()?;
    let day = resolve_day(&root, day)?;
    let crate_name = given_crate_name_or_guessed_from_day(day, crate_name)?;

    let path = root.join(&crate_name);
    if path.exists() {
        bail!("ERROR: {crate_name} already exists");
    }
    fs::create_dir(&path)?;
    fs::create_dir(path.join("src"))?;

    fs::write(
        path.join("Cargo.toml"),
        CARGO_TOML.replace("<name>", &crate_name).trim(),
    )?;
    fs::write(path.join("src/main.rs"), MAIN_RS.trim())?;

    add_to_workspace(&root, &crate_name)?;
    download_input(&root, &crate_name, day)?;

    let metadata = DayMetadata {
        day,
        part_1_complete: false,
        part_2_complete: false,
    };
    metadata.write(&path)?;

    println!("Created new day at {} for day {day}", path.display());

    Ok(())
}

#[derive(Deserialize)]
struct Answer {
    part: u32,
    answer: String,
}

fn run_program(root: &Path, crate_name: &str) -> Result<HashMap<u32, String>> {
    let mut input = fs::File::open(root.join(crate_name).join("input.txt"))
        .with_context(|| "input.txt not found")?;

    let mut child = Command::new("cargo")
        .args(["run", "--bin", crate_name, "--", "--json"])
        .current_dir(root)
        .stdout(Stdio::piped())
        .stdin(Stdio::piped())
        .spawn()
        .with_context(|| format!("failed to run program for {crate_name}"))?;

    std::thread::scope(|scope| {
        let mut stdin = child.stdin.take().unwrap();
        let stdin_copy = scope.spawn(move || std::io::copy(&mut input, &mut stdin));

        let mut answers = HashMap::new();
        for line in BufReader::new(child.stdout.take().unwrap()).lines() {
            let line = line.with_context(|| "failed reading program output")?;
            let answer = serde_json::from_str::<Answer>(&line)
                .with_context(|| "failed decoding program output")?;
            if answers.insert(answer.part, answer.answer).is_some() {
                bail!(
                    "program for {crate_name} produced two answers for part {}",
                    answer.part
                );
            }
        }

        let exit_status = child.wait()?;
        if !exit_status.success() {
            if let Some(exit_code) = exit_status.code() {
                bail!(
                    "running program for {crate_name} failed with non-zero exit code {exit_code}"
                );
            } else {
                bail!("running program for {crate_name} failed with signal");
            }
        }

        stdin_copy
            .join()
            .unwrap()
            .with_context(|| "failed to write to program stdin")?;

        Ok(answers)
    })
}

const ALREADY_COMPLETE: &str = "You don't seem to be solving the right level.";
const SUCCESS: &str = "That's the right answer!";
const FAIL: &str = "That's not the right answer.";
const FAIL_HIGH: &str = "That's not the right answer; your answer is too high.";
const FAIL_LOW: &str = "That's not the right answer; your answer is too low.";
const FAIL_TIMEOUT: &str = "You gave an answer too recently";

const REMOTE_MESSAGES: [&str; 6] = [
    ALREADY_COMPLETE,
    SUCCESS,
    FAIL,
    FAIL_HIGH,
    FAIL_LOW,
    FAIL_TIMEOUT,
];

fn send_answer(
    root: &Path,
    day: u32,
    part: u32,
    result: &str,
    dry_run: bool,
) -> Result<Option<String>> {
    let year = aoc_year(root)?;
    let url = format!("https://adventofcode.com/{year}/day/{day}/answer");
    let body = format!("level={part}&answer={result}");
    if dry_run {
        println!("POST {url} {body}");
        Ok(None)
    } else {
        let client = reqwest::blocking::Client::new();
        let resp = client
            .post(url)
            .header("Content-Type", "application/x-www-form-urlencoded")
            .header("cookie", format!("session={}", aoc_token()?))
            .body(body)
            .send()?;
        Ok(Some(resp.text()?))
    }
}

fn mark_day_completed(mut metadata: DayMetadata, path: &Path, part: u32) -> Result<()> {
    if part == 1 {
        metadata.part_1_complete = true;
    } else if part == 2 {
        metadata.part_2_complete = true;
    }
    metadata.write(path)?;

    Ok(())
}

fn find_day_to_submit(
    root: &Path,
    crate_name: &Option<String>,
    day: Option<u32>,
) -> Result<(u32, PathBuf, DayMetadata)> {
    // If we gave a crate name, trust it
    if let Some(crate_name) = crate_name {
        let path = root.join(crate_name);
        let metadata = DayMetadata::read(&path)?;
        // if we also gave a day, override whatever day the crate says it is
        Ok((day.unwrap_or(metadata.day), path, metadata))
    } else {
        let mut days = get_existing_days(root)?;
        // If we gave a day number, find the crate that matches
        if let Some(day) = day {
            let (p, m) = days
                .remove(&day)
                .ok_or_else(|| anyhow!("couldn't find day {day}"))?;
            Ok((day, p, m))
        } else {
            // Otherwise, search for earliest day with un-submitted parts
            while let Some((_, (p, m))) = days.pop_first() {
                if !m.part_1_complete || !m.part_2_complete {
                    return Ok((m.day, p, m));
                }
            }
            bail!("couldn't find a day to submit")
        }
    }
}

fn submit_answer(
    crate_name: &Option<String>,
    day: Option<u32>,
    part: Option<u32>,
    dry_run: bool,
) -> Result<()> {
    let root = find_aoc_root()?;
    let (day, path, metadata) = find_day_to_submit(&root, crate_name, day)?;
    let crate_name = path.file_name().unwrap().to_str().unwrap();
    let part = if let Some(part) = part {
        part
    } else {
        metadata.next_part()?
    };

    println!("submitting output of {crate_name} for day {day} part {part}");

    let answers = run_program(&root, crate_name)?;
    let result = answers
        .get(&part)
        .ok_or_else(|| anyhow!("program was missing answer for part {part}"))?;
    let Some(reply) = send_answer(&root, day, part, result, dry_run)? else {
        return Ok(());
    };

    if let Some(m) = REMOTE_MESSAGES.iter().find(|&m| reply.contains(m)) {
        println!("remote: {m}");
        if m == &SUCCESS {
            mark_day_completed(metadata, &path, part)?;
        }
    } else {
        println!("{reply}");
    }

    Ok(())
}

fn find_aoc_root() -> Result<PathBuf> {
    let mut p = std::env::current_dir()?;

    loop {
        if p.join(AOC_YEAR).exists() {
            return Ok(p);
        }
        if !p.pop() {
            bail!("Could not find {AOC_YEAR} file");
        }
    }
}

fn init(year: u32) -> Result<()> {
    fs::write(AOC_YEAR, year.to_string())?;
    fs::remove_dir_all("support")?;

    let exit_status = Command::new("git")
        .args([
            "submodule",
            "add",
            "https://github.com/bobbobbio/Advent-of-Code-Support",
            "support",
        ])
        .status()?;
    if !exit_status.success() {
        if let Some(exit_code) = exit_status.code() {
            bail!("running `git add submodule` failed with non-zero exit code {exit_code}");
        } else {
            bail!("running `git add submodule` failed with signal");
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let opt = CliOptions::parse();
    match opt.command {
        CliCommand::NewDay(opt) => add_new_day(&opt.crate_name, opt.day)?,
        CliCommand::Submit(opt) => submit_answer(&opt.crate_name, opt.day, opt.part, opt.dry_run)?,
        CliCommand::Init(opt) => init(opt.year)?,
    }

    Ok(())
}

use anyhow::{anyhow, bail, Context as _, Result};
use clap::{Parser, Subcommand};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::io::{BufRead as _, BufReader};
use std::path::Path;
use std::process::{Command, Stdio};

#[derive(Parser)]
struct CliOptions {
    #[command(subcommand)]
    command: CliCommand,
}

#[derive(Subcommand)]
enum CliCommand {
    NewQuestion(NewQuestionOptions),
    Submit(SubmitOptions),
}

#[derive(Parser)]
struct NewQuestionOptions {
    #[arg(long)]
    name: String,
    #[arg(long)]
    day: Option<u32>,
}

impl NewQuestionOptions {
    fn day(&self) -> Result<u32> {
        Ok(match self.day {
            Some(day) => day,
            None => word_to_num(&self.name)
                .ok_or_else(|| anyhow!("day number could not be determined"))?,
        })
    }
}

#[derive(Parser)]
struct SubmitOptions {
    #[arg(long)]
    name: String,
    #[arg(long)]
    day: Option<u32>,
    #[arg(long)]
    part: u32,
    #[arg(long, action)]
    dry_run: bool,
}

impl SubmitOptions {
    fn day(&self) -> Result<u32> {
        Ok(match self.day {
            Some(day) => day,
            None => word_to_num(&self.name)
                .ok_or_else(|| anyhow!("day number could not be determined"))?,
        })
    }
}

fn word_to_num(word: &str) -> Option<u32> {
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
    for (i, w) in WORDS.iter().enumerate() {
        if w == &word {
            return Some(i as u32 + 1);
        }
    }
    None
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

fn aoc_year() -> Result<u32> {
    Ok(fs::read_to_string("aoc-year")
        .with_context(|| "year missing: ./aoc-year")?
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

fn download_input(name: &str, day: u32) -> Result<()> {
    let year = aoc_year()?;
    let client = reqwest::blocking::Client::new();
    let resp = client
        .get(format!("https://adventofcode.com/{year}/day/{day}/input"))
        .header("cookie", format!("session={}", aoc_token()?))
        .send()?;

    let input_text = resp.text()?;
    if input_text == "Puzzle inputs differ by user.  Please log in to get your puzzle input." {
        bail!("session key invalid");
    }

    fs::write(Path::new(name).join("input.txt"), input_text)?;
    Ok(())
}

fn add_to_workspace(name: &str) -> Result<()> {
    let toml = fs::read_to_string("Cargo.toml")?;
    let mut cargo_toml: toml::Value = toml::from_str(&toml)?;
    let members = cargo_toml["workspace"]["members"]
        .as_array_mut()
        .ok_or_else(|| anyhow!("workspace.members present"))?;
    members.push(name.into());
    members.sort_by_key(|v| v.as_str().map(|s| s.to_string()).unwrap_or_default());

    fs::write("Cargo.toml", toml::to_string_pretty(&cargo_toml)?)?;

    Ok(())
}

fn add_new_question(name: &str, day: u32) -> Result<()> {
    let path = Path::new(name);
    if path.exists() {
        bail!("ERROR: {name} already exists");
    }
    fs::create_dir(path)?;
    fs::create_dir(path.join("src"))?;

    fs::write(
        path.join("Cargo.toml"),
        CARGO_TOML.replace("<name>", name).trim(),
    )?;
    fs::write(path.join("src/main.rs"), MAIN_RS.trim())?;

    add_to_workspace(name)?;
    download_input(name, day)?;

    Ok(())
}

#[derive(Deserialize)]
struct Answer {
    part: u32,
    answer: String,
}

fn run_program(name: &str) -> Result<HashMap<u32, String>> {
    let mut input =
        fs::File::open(Path::new(name).join("input.txt")).with_context(|| "input.txt not found")?;

    let mut child = Command::new("cargo")
        .args(["run", "--bin", name, "--", "--json"])
        .stdout(Stdio::piped())
        .stdin(Stdio::piped())
        .spawn()
        .with_context(|| format!("failed to run program for {name}"))?;

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
                    "program for {name} produced two answers for part {}",
                    answer.part
                );
            }
        }

        let exit_status = child.wait()?;
        if !exit_status.success() {
            if let Some(exit_code) = exit_status.code() {
                bail!("running program for {name} failed with non-zero exit code {exit_code}");
            } else {
                bail!("running program for {name} failed with signal");
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

fn send_answer(day: u32, part: u32, result: &str, dry_run: bool) -> Result<Option<String>> {
    let year = aoc_year()?;
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

fn submit_answer(name: &str, day: u32, part: u32, dry_run: bool) -> Result<()> {
    let answers = run_program(name)?;
    let result = answers
        .get(&part)
        .ok_or_else(|| anyhow!("program was missing answer for part {part}"))?;
    let Some(reply) = send_answer(day, part, result, dry_run)? else {
        return Ok(());
    };

    if let Some(m) = REMOTE_MESSAGES.iter().find(|&m| reply.contains(m)) {
        println!("remote: {m}");
    } else {
        println!("{reply}");
    }

    Ok(())
}

fn main() -> Result<()> {
    let opt = CliOptions::parse();
    match opt.command {
        CliCommand::NewQuestion(opt) => add_new_question(&opt.name, opt.day()?)?,
        CliCommand::Submit(opt) => submit_answer(&opt.name, opt.day()?, opt.part, opt.dry_run)?,
    }

    Ok(())
}

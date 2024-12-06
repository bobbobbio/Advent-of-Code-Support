#![warn(missing_docs)]

//! This crate contains code that helps with solving [Advent of Code](https://adventofcode.com)
//! problems using Rust.

/// This module can be imported like
/// ```rust
/// use advent::prelude::*;
/// ```
/// to import a bunch of useful things.
pub mod prelude {
    pub use super::Grid;
    pub use advent_macro::*;
    pub use parse::prelude::*;
    pub use std::collections::*;
}
pub use combine;
pub use parse;

use std::fmt;
use std::marker::PhantomData;

/// Represents one horizontal row of a [`Grid`]
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Row<CellT>(Vec<CellT>);

impl<CellT> Row<CellT> {
    /// Returns the number of cells in the row
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the row contains no cells
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns an iterator which yields shared references to the cells in the row
    pub fn iter(&self) -> std::slice::Iter<'_, CellT> {
        self.0.iter()
    }

    /// Returns an iterator which yields mutable references to the cells in the row
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, CellT> {
        self.0.iter_mut()
    }

    /// Clone the contents of the row and return as a [`Vec`]
    pub fn to_vec(&self) -> Vec<CellT>
    where
        CellT: Clone,
    {
        self.iter().cloned().collect()
    }
}

impl<CellT> IntoIterator for Row<CellT> {
    type Item = CellT;
    type IntoIter = std::vec::IntoIter<CellT>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, CellT> IntoIterator for &'a Row<CellT> {
    type Item = &'a CellT;
    type IntoIter = std::slice::Iter<'a, CellT>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, CellT> IntoIterator for &'a mut Row<CellT> {
    type Item = &'a mut CellT;
    type IntoIter = std::slice::IterMut<'a, CellT>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<CellT> std::ops::Index<usize> for Row<CellT> {
    type Output = CellT;

    fn index(&self, index: usize) -> &CellT {
        &self.0[index]
    }
}

impl<CellT> std::ops::IndexMut<usize> for Row<CellT> {
    fn index_mut(&mut self, index: usize) -> &mut CellT {
        &mut self.0[index]
    }
}

#[doc(hidden)]
pub trait GetColumnCell {
    type Cell: Sized;

    fn get_cell(&self, index: usize) -> Self::Cell;
}

impl<'column, 'grid, CellT> GetColumnCell for &'column Column<'grid, CellT> {
    type Cell = &'column CellT;

    fn get_cell(&self, index: usize) -> &'column CellT {
        &self[index]
    }
}

impl<'grid, CellT> GetColumnCell for Column<'grid, CellT> {
    type Cell = &'grid CellT;

    fn get_cell(&self, index: usize) -> &'grid CellT {
        &self.grid.row(index)[self.column]
    }
}

/// An iterator over the cells of a [`Grid`] that goes from top to bottom. Yields shared
/// references, if you need mutable references see [`VerticalCellIterMut`].
pub struct VerticalCellIter<ColumnT> {
    column: ColumnT,
    row_forward: usize,
    row_backward: usize,
}

impl<ColumnT: GetColumnCell> Iterator for VerticalCellIter<ColumnT> {
    type Item = ColumnT::Cell;

    fn next(&mut self) -> Option<Self::Item> {
        if self.row_forward >= self.row_backward {
            None
        } else {
            let item = self.column.get_cell(self.row_forward);
            self.row_forward += 1;
            Some(item)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.row_backward - self.row_forward;
        (len, Some(len))
    }
}

impl<ColumnT: GetColumnCell> DoubleEndedIterator for VerticalCellIter<ColumnT> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.row_backward == self.row_forward {
            None
        } else {
            self.row_backward -= 1;
            Some(self.column.get_cell(self.row_backward))
        }
    }
}

impl<ColumnT: GetColumnCell> ExactSizeIterator for VerticalCellIter<ColumnT> {}

/// Represents one column of a [`Grid`].
///
/// This type is only able to yield shared references to cells. If you want to mutate a column you
/// need a [`ColumnMut`] instead
pub struct Column<'grid, CellT> {
    grid: &'grid Grid<CellT>,
    column: usize,
}

impl<'grid, CellT> Column<'grid, CellT> {
    /// Returns the number of cells in this column
    pub fn len(&self) -> usize {
        self.grid.height()
    }

    /// Returns true if the column contains no cells
    pub fn is_empty(&self) -> bool {
        self.grid.is_empty()
    }

    /// Returns an iterator which yields shared references to the cells in the column
    pub fn iter(&self) -> VerticalCellIter<&'_ Self> {
        VerticalCellIter {
            column: self,
            row_forward: 0,
            row_backward: self.len(),
        }
    }

    /// Clone the contents of the column and return as a [`Vec`]
    pub fn to_vec(&self) -> Vec<CellT>
    where
        CellT: Clone,
    {
        self.iter().cloned().collect()
    }
}

impl<'a, 'grid, CellT> IntoIterator for &'a Column<'grid, CellT> {
    type Item = &'a CellT;
    type IntoIter = VerticalCellIter<Self>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'grid, CellT> IntoIterator for Column<'grid, CellT> {
    type Item = &'grid CellT;
    type IntoIter = VerticalCellIter<Self>;

    fn into_iter(self) -> Self::IntoIter {
        VerticalCellIter {
            row_forward: 0,
            row_backward: self.len(),
            column: self,
        }
    }
}

impl<'grid, CellT> std::ops::Index<usize> for Column<'grid, CellT> {
    type Output = CellT;

    fn index(&self, index: usize) -> &CellT {
        &self.grid[index][self.column]
    }
}

/// An iterator that yields the columns of a [`Grid`]
///
/// The columns this yields can only yield shared references to cells.
pub struct ColumnIter<'grid, CellT> {
    grid: &'grid Grid<CellT>,
    column_forward: usize,
    column_backward: usize,
}

impl<'grid, CellT> ColumnIter<'grid, CellT> {
    fn new(grid: &'grid Grid<CellT>) -> Self {
        Self {
            grid,
            column_forward: 0,
            column_backward: grid.width(),
        }
    }
}

impl<'grid, CellT> Iterator for ColumnIter<'grid, CellT> {
    type Item = Column<'grid, CellT>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.column_forward < self.column_backward {
            let item = Column {
                grid: self.grid,
                column: self.column_forward,
            };
            self.column_forward += 1;
            Some(item)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.column_backward - self.column_forward;
        (len, Some(len))
    }
}

impl<'grid, CellT> DoubleEndedIterator for ColumnIter<'grid, CellT> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.column_backward == self.column_forward {
            None
        } else {
            self.column_backward -= 1;
            Some(Column {
                grid: self.grid,
                column: self.column_backward,
            })
        }
    }
}

impl<'grid, CellT> ExactSizeIterator for ColumnIter<'grid, CellT> {}

#[doc(hidden)]
pub trait GetColumnCellMut {
    type Cell: Sized;

    fn get_cell_mut(&mut self, index: usize) -> Self::Cell;
}

impl<'column, 'grid, CellT> GetColumnCellMut for &'column mut ColumnMut<'grid, CellT> {
    type Cell = &'column mut CellT;

    fn get_cell_mut(&mut self, index: usize) -> &'column mut CellT {
        self.cell_mut(index)
    }
}

impl<'grid, CellT> GetColumnCellMut for ColumnMut<'grid, CellT> {
    type Cell = &'grid mut CellT;

    fn get_cell_mut(&mut self, index: usize) -> &'grid mut CellT {
        self.cell_mut(index)
    }
}

impl<'column, 'grid, CellT> GetColumnCell for &'column ColumnMut<'grid, CellT> {
    type Cell = &'column CellT;

    fn get_cell(&self, index: usize) -> &'column CellT {
        self.cell(index)
    }
}

impl<'grid, CellT> GetColumnCell for ColumnMut<'grid, CellT> {
    type Cell = &'grid CellT;

    fn get_cell(&self, index: usize) -> &'grid CellT {
        self.cell(index)
    }
}

/// An iterator over the cells of a [`Grid`] that goes from top to bottom. Yields mutable
/// references.
pub struct VerticalCellIterMut<ColumnT> {
    column: ColumnT,
    row_forward: usize,
    row_backward: usize,
}

impl<ColumnT: GetColumnCellMut> Iterator for VerticalCellIterMut<ColumnT> {
    type Item = ColumnT::Cell;

    fn next(&mut self) -> Option<Self::Item> {
        if self.row_forward >= self.row_backward {
            None
        } else {
            let item = self.column.get_cell_mut(self.row_forward);
            self.row_forward += 1;
            Some(item)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.row_backward - self.row_forward;
        (len, Some(len))
    }
}

impl<ColumnT: GetColumnCellMut> DoubleEndedIterator for VerticalCellIterMut<ColumnT> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.row_backward == self.row_forward {
            None
        } else {
            self.row_backward -= 1;
            Some(self.column.get_cell_mut(self.row_backward))
        }
    }
}

impl<ColumnT: GetColumnCellMut> ExactSizeIterator for VerticalCellIterMut<ColumnT> {}

/// Represents one column of a [`Grid`].
///
/// This type is able to yield mutable references to cells.
pub struct ColumnMut<'grid, CellT> {
    grid_ptr: *mut Grid<CellT>,
    grid: PhantomData<&'grid mut Grid<CellT>>,
    column: usize,
}

impl<'grid, CellT> ColumnMut<'grid, CellT> {
    fn new(grid: &'grid mut Grid<CellT>, column: usize) -> Self {
        Self {
            grid_ptr: grid,
            grid: PhantomData,
            column,
        }
    }

    /// Returns the number of cells in this column
    pub fn len(&self) -> usize {
        unsafe { &*self.grid_ptr }.height()
    }

    /// Returns true if the column contains no cells
    pub fn is_empty(&self) -> bool {
        unsafe { &*self.grid_ptr }.is_empty()
    }

    /// Returns an iterator which yields shared references to the cells in the column
    pub fn iter(&self) -> VerticalCellIter<&'_ Self> {
        VerticalCellIter {
            row_forward: 0,
            row_backward: self.len(),
            column: self,
        }
    }

    /// Returns an iterator which yields mutable references to the cells in the column
    pub fn iter_mut(&mut self) -> VerticalCellIterMut<&'_ mut Self> {
        VerticalCellIterMut {
            row_forward: 0,
            row_backward: self.len(),
            column: self,
        }
    }

    /// Clone the contents of the column and return as a [`Vec`]
    pub fn to_vec(&self) -> Vec<CellT>
    where
        CellT: Clone,
    {
        self.iter().cloned().collect()
    }

    fn cell(&self, index: usize) -> &'grid CellT {
        &unsafe { &*self.grid_ptr }.row(index).0[self.column]
    }

    fn cell_mut(&mut self, index: usize) -> &'grid mut CellT {
        &mut unsafe { &mut *self.grid_ptr }.row_mut(index).0[self.column]
    }
}

impl<'a, 'grid, CellT> IntoIterator for &'a ColumnMut<'grid, CellT> {
    type Item = &'a CellT;
    type IntoIter = VerticalCellIter<Self>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, 'grid, CellT> IntoIterator for &'a mut ColumnMut<'grid, CellT> {
    type Item = &'a mut CellT;
    type IntoIter = VerticalCellIterMut<Self>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'grid, CellT> IntoIterator for ColumnMut<'grid, CellT> {
    type Item = &'grid mut CellT;
    type IntoIter = VerticalCellIterMut<Self>;

    fn into_iter(self) -> Self::IntoIter {
        VerticalCellIterMut {
            row_forward: 0,
            row_backward: self.len(),
            column: self,
        }
    }
}

impl<'grid, CellT> std::ops::Index<usize> for ColumnMut<'grid, CellT> {
    type Output = CellT;

    fn index(&self, index: usize) -> &CellT {
        self.cell(index)
    }
}

impl<'grid, CellT> std::ops::IndexMut<usize> for ColumnMut<'grid, CellT> {
    fn index_mut(&mut self, index: usize) -> &mut CellT {
        self.cell_mut(index)
    }
}

/// An iterator that yields the columns of a [`Grid`]
///
/// The columns this yields yield mutable references to cells.
pub struct ColumnIterMut<'grid, CellT> {
    grid_ptr: *mut Grid<CellT>,
    grid: PhantomData<&'grid mut Grid<CellT>>,
    column_forward: usize,
    column_backward: usize,
}

impl<'grid, CellT> ColumnIterMut<'grid, CellT> {
    fn new(grid: &'grid mut Grid<CellT>) -> Self {
        Self {
            grid_ptr: grid,
            grid: PhantomData,
            column_forward: 0,
            column_backward: grid.width(),
        }
    }
}

impl<'grid, CellT> Iterator for ColumnIterMut<'grid, CellT> {
    type Item = ColumnMut<'grid, CellT>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.column_forward < self.column_backward {
            let item =
                ColumnMut::<'grid, CellT>::new(unsafe { &mut *self.grid_ptr }, self.column_forward);
            self.column_forward += 1;
            Some(item)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.column_backward - self.column_forward;
        (len, Some(len))
    }
}

impl<'grid, CellT> DoubleEndedIterator for ColumnIterMut<'grid, CellT> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.column_backward == self.column_forward {
            None
        } else {
            self.column_backward -= 1;
            Some(ColumnMut::<'grid, CellT>::new(
                unsafe { &mut *self.grid_ptr },
                self.column_backward,
            ))
        }
    }
}

impl<'grid, CellT> ExactSizeIterator for ColumnIterMut<'grid, CellT> {}

/// An iterator that yields mutable references to all cells of a [`Grid`]
pub struct CellIterMut<'grid, CellT> {
    grid_ptr: *mut Grid<CellT>,
    grid: PhantomData<&'grid mut Grid<CellT>>,
    forward: usize,
    backward: usize,
}

impl<'grid, CellT> CellIterMut<'grid, CellT> {
    fn new(grid: &'grid mut Grid<CellT>) -> Self {
        Self {
            grid_ptr: grid,
            grid: PhantomData,
            forward: 0,
            backward: grid.width() * grid.height(),
        }
    }
}

impl<'grid, CellT> Iterator for CellIterMut<'grid, CellT> {
    type Item = &'grid mut CellT;

    fn next(&mut self) -> Option<Self::Item> {
        if self.forward < self.backward {
            let (y, x) = {
                let grid: &'grid Grid<CellT> = unsafe { &*self.grid_ptr };
                (self.forward / grid.width(), self.forward % grid.width())
            };
            self.forward += 1;
            Some(&mut unsafe { &mut *self.grid_ptr }.row_mut(y).0[x])
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.backward - self.forward;
        (len, Some(len))
    }
}

impl<'grid, CellT> DoubleEndedIterator for CellIterMut<'grid, CellT> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.backward == self.forward {
            None
        } else {
            self.backward -= 1;
            let (y, x) = {
                let grid: &'grid Grid<CellT> = unsafe { &*self.grid_ptr };
                (self.forward / grid.width(), self.forward % grid.width())
            };
            Some(&mut unsafe { &mut *self.grid_ptr }.row_mut(y).0[x])
        }
    }
}

impl<'grid, CellT> ExactSizeIterator for CellIterMut<'grid, CellT> {}

/// An iterator that yields shared references to all cells of a [`Grid`]
pub struct CellIter<'grid, CellT> {
    grid: &'grid Grid<CellT>,
    forward: usize,
    backward: usize,
}

impl<'grid, CellT> CellIter<'grid, CellT> {
    fn new(grid: &'grid Grid<CellT>) -> Self {
        Self {
            grid,
            forward: 0,
            backward: grid.width() * grid.height(),
        }
    }
}

impl<'grid, CellT> Iterator for CellIter<'grid, CellT> {
    type Item = &'grid CellT;

    fn next(&mut self) -> Option<Self::Item> {
        if self.forward < self.backward {
            let item = &self.grid[self.forward / self.grid.width()][self.forward % self.grid.width()];
            self.forward += 1;
            Some(item)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.backward - self.forward;
        (len, Some(len))
    }
}

impl<'grid, CellT> DoubleEndedIterator for CellIter<'grid, CellT> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.backward == self.forward {
            None
        } else {
            self.backward -= 1;
            Some(&self.grid[self.backward / self.grid.width()][self.backward % self.grid.width()])
        }
    }
}

impl<'grid, CellT> ExactSizeIterator for CellIter<'grid, CellT> {}

/// A simple 2-D array type.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Grid<CellT>(Vec<Row<CellT>>);

impl<CellT> Grid<CellT> {
    /// Create a new Grid from the given 2-D Vec. It returns `None` if the given rows are not all
    /// the same length. It also returns `None` if any of the rows are empty.
    pub fn new(v: Vec<Vec<CellT>>) -> Option<Self> {
        if !v.is_empty() {
            let first_len = v[0].len();
            if first_len == 0 {
                return None;
            }

            if !v.iter().all(|v| v.len() == first_len) {
                return None;
            }
        }
        Some(Self(v.into_iter().map(|v| Row(v)).collect()))
    }

    /// The height of the grid in number of cells
    pub fn height(&self) -> usize {
        self.0.len()
    }

    /// The width of the grid in number of cells
    pub fn width(&self) -> usize {
        if self.0.is_empty() {
            0
        } else {
            self.0[0].len()
        }
    }

    /// An iterator over the rows in the grid. The rows it yields can only yield shared references
    /// to cells.
    pub fn rows(&self) -> std::slice::Iter<'_, Row<CellT>> {
        self.0.iter()
    }

    /// An iterator over the columns in the grid. The column it yields can only yield shared
    /// references to cells.
    pub fn columns(&self) -> ColumnIter<'_, CellT> {
        ColumnIter::new(self)
    }

    /// Get a shared reference to an individual row.
    pub fn row(&self, index: usize) -> &Row<CellT> {
        &self.0[index]
    }

    /// Get an individual column. This column can only yield shared references to cells.
    pub fn column(&self, index: usize) -> Column<'_, CellT> {
        Column {
            grid: self,
            column: index,
        }
    }

    /// An iterator over the rows in the grid. The rows it yields is able to yield mutable
    /// references to cells.
    pub fn rows_mut(&mut self) -> std::slice::IterMut<'_, Row<CellT>> {
        self.0.iter_mut()
    }

    /// An iterator over the columns in the grid. The column it yields is able to yield mutable
    /// references to cells.
    pub fn columns_mut(&mut self) -> ColumnIterMut<'_, CellT> {
        ColumnIterMut::new(self)
    }

    /// Get a mutable reference to an individual row.
    pub fn row_mut(&mut self, index: usize) -> &mut Row<CellT> {
        &mut self.0[index]
    }

    /// Get an individual column. This column is able to yield mutable references to cells.
    pub fn column_mut(&mut self, index: usize) -> ColumnMut<'_, CellT> {
        ColumnMut::new(self, index)
    }

    /// Returns true if the grid has no rows.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// An iterator over all the cells in the Grid which yields shared references.
    pub fn cells(&self) -> CellIter<'_, CellT> {
        CellIter::new(self)
    }

    /// An iterator over all the cells in the Grid which yields mutable references.
    pub fn cells_mut(&mut self) -> CellIterMut<'_, CellT> {
        CellIterMut::new(self)
    }

    /// Find a cell matching the given pattern. Returns index as `Some((row, column))` if found,
    /// and `None` otherwise.
    pub fn position(&self, mut pattern: impl FnMut(&CellT) -> bool) -> Option<(usize, usize)> {
        for r in 0..self.height() {
            for c in 0..self.width() {
                if pattern(&self[r][c]) {
                    return Some((r, c))
                }
            }
        }
        None
    }
}

impl<CellT> std::ops::Index<usize> for Grid<CellT> {
    type Output = Row<CellT>;

    fn index(&self, index: usize) -> &Row<CellT> {
        self.row(index)
    }
}

impl<CellT> std::ops::IndexMut<usize> for Grid<CellT> {
    fn index_mut(&mut self, index: usize) -> &mut Row<CellT> {
        self.row_mut(index)
    }
}

impl<CellT: fmt::Display> fmt::Display for Grid<CellT> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            writeln!(f, "<empty>")?;
            return Ok(());
        }

        for row in self.rows() {
            for c in row.iter() {
                write!(f, "{c}")?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<CellT: parse::HasParser> parse::HasParser for Grid<CellT> {
    #[parse::prelude::into_parser]
    fn parser() -> _ {
        use combine::error::Commit;
        use parse::prelude::*;

        combine::parser(|input: &mut Input| {
            let position = input.position();
            let mut parser = many1::<Vec<Vec<_>>, _, _>(many1(CellT::parser()).skip(string("\n")));
            let (rows, committed) = parser.parse_stream(input).into_result()?;
            if let Some(grid) = Grid::new(rows) {
                Ok((grid, committed))
            } else {
                let mut errors = Input::Error::empty(position);
                errors.add_message("inconsistent grid row length");
                Err(Commit::Peek(errors.into()))
            }
        })
    }
}

#[test]
fn grid_new() {
    assert!(Grid::new(vec![vec![1, 2], vec![1, 2, 3]]).is_none());
    assert!(Grid::<i32>::new(vec![vec![], vec![]]).is_none());

    let empty_grid = Grid::<i32>::new(vec![]).unwrap();
    assert!(empty_grid.is_empty());

    let non_empty_grid = Grid::new(vec![vec![1, 2, 3]]).unwrap();
    assert!(!non_empty_grid.is_empty());
}

#[test]
fn grid_row_iter() {
    let grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7]]).unwrap();
    let mut rows = grid.rows();

    let row0 = rows.next().unwrap();
    assert_eq!(Vec::from_iter(row0.iter().copied()), vec![0, 1, 2, 3]);

    let row1 = rows.next().unwrap();
    assert_eq!(Vec::from_iter(row1.iter().copied()), vec![4, 5, 6, 7]);
    assert!(rows.next().is_none());

    assert_eq!(
        Vec::from_iter(grid.row(1).iter().copied().rev()),
        vec![7, 6, 5, 4]
    );

    let row = grid.row(1);
    let mut iter = row.iter().copied();
    assert_eq!(iter.len(), 4);
    assert_eq!(iter.next().unwrap(), 4);

    let mut iter = iter.rev();
    assert_eq!(iter.len(), 3);
    assert_eq!(iter.next().unwrap(), 7);
    assert_eq!(iter.len(), 2);
    assert_eq!(iter.next().unwrap(), 6);
    assert_eq!(iter.len(), 1);
    assert_eq!(iter.next().unwrap(), 5);
    assert_eq!(iter.len(), 0);
    assert!(iter.next().is_none());
}

#[test]
fn grid_row_iter_mut() {
    let mut grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7]]).unwrap();
    let mut rows = grid.rows_mut();

    let row0 = rows.next().unwrap();
    assert_eq!(
        Vec::from_iter(row0.iter_mut().map(|c| *c)),
        vec![0, 1, 2, 3]
    );

    let row1 = rows.next().unwrap();
    assert_eq!(
        Vec::from_iter(row1.iter_mut().map(|c| *c)),
        vec![4, 5, 6, 7]
    );
    assert!(rows.next().is_none());

    assert_eq!(
        Vec::from_iter(grid.row_mut(1).iter_mut().map(|c| *c).rev()),
        vec![7, 6, 5, 4]
    );

    let row = grid.row_mut(1);
    let mut iter = row.iter_mut().map(|c| *c);
    assert_eq!(iter.len(), 4);
    assert_eq!(iter.next().unwrap(), 4);

    let mut iter = iter.rev();
    assert_eq!(iter.len(), 3);
    assert_eq!(iter.next().unwrap(), 7);
    assert_eq!(iter.len(), 2);
    assert_eq!(iter.next().unwrap(), 6);
    assert_eq!(iter.len(), 1);
    assert_eq!(iter.next().unwrap(), 5);
    assert_eq!(iter.len(), 0);
    assert!(iter.next().is_none());

    for row in grid.rows_mut() {
        for c in row {
            *c += 1;
        }
    }
    assert_eq!(
        grid,
        Grid::new(vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8]]).unwrap()
    );
}

#[test]
fn grid_column_iter() {
    let grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7], vec![8, 9, 10, 11]]).unwrap();
    let mut columns = grid.columns();
    let column0 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column0.iter().copied()), vec![0, 4, 8]);

    let column1 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column1.iter().copied()), vec![1, 5, 9]);

    let column2 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column2.iter().copied()), vec![2, 6, 10]);

    let column3 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column3.iter().copied()), vec![3, 7, 11]);

    assert!(columns.next().is_none());

    assert_eq!(
        Vec::from_iter(grid.column(1).iter().copied().rev()),
        vec![9, 5, 1]
    );

    let column = grid.column(1);
    let mut iter = column.iter().copied();

    assert_eq!(iter.len(), 3);
    assert_eq!(iter.next().unwrap(), 1);

    let mut iter = iter.rev();
    assert_eq!(iter.len(), 2);
    assert_eq!(iter.next().unwrap(), 9);
    assert_eq!(iter.len(), 1);
    assert_eq!(iter.next().unwrap(), 5);
    assert_eq!(iter.len(), 0);
    assert!(iter.next().is_none());
}

#[test]
fn grid_column_into_iter() {
    let grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7], vec![8, 9, 10, 11]]).unwrap();
    let mut columns = grid.columns();
    let column0 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column0.into_iter().copied()), vec![0, 4, 8]);

    let column1 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column1.into_iter().copied()), vec![1, 5, 9]);

    let column2 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column2.into_iter().copied()), vec![2, 6, 10]);

    let column3 = columns.next().unwrap();
    assert_eq!(Vec::from_iter(column3.into_iter().copied()), vec![3, 7, 11]);

    assert!(columns.next().is_none());

    assert_eq!(
        Vec::from_iter(grid.column(1).into_iter().copied().rev()),
        vec![9, 5, 1]
    );

    let column = grid.column(1);
    let mut iter = column.into_iter().copied();

    assert_eq!(iter.len(), 3);
    assert_eq!(iter.next().unwrap(), 1);

    let mut iter = iter.rev();
    assert_eq!(iter.len(), 2);
    assert_eq!(iter.next().unwrap(), 9);
    assert_eq!(iter.len(), 1);
    assert_eq!(iter.next().unwrap(), 5);
    assert_eq!(iter.len(), 0);
    assert!(iter.next().is_none());
}

#[test]
fn grid_column_iter_mut() {
    let mut grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7], vec![8, 9, 10, 11]]).unwrap();
    let mut columns = grid.columns_mut();
    let mut column0 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column0.iter_mut().map(|c| *c)),
        vec![0, 4, 8]
    );

    let mut column1 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column1.iter_mut().map(|c| *c)),
        vec![1, 5, 9]
    );

    let mut column2 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column2.iter_mut().map(|c| *c)),
        vec![2, 6, 10]
    );

    let mut column3 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column3.iter_mut().map(|c| *c)),
        vec![3, 7, 11]
    );

    assert!(columns.next().is_none());

    assert_eq!(
        Vec::from_iter(grid.column_mut(1).iter_mut().map(|c| *c).rev()),
        vec![9, 5, 1]
    );

    let mut column = grid.column_mut(1);
    let mut iter = column.iter_mut().map(|c| *c);

    assert_eq!(iter.len(), 3);
    assert_eq!(iter.next().unwrap(), 1);

    let mut iter = iter.rev();
    assert_eq!(iter.len(), 2);
    assert_eq!(iter.next().unwrap(), 9);
    assert_eq!(iter.len(), 1);
    assert_eq!(iter.next().unwrap(), 5);
    assert_eq!(iter.len(), 0);
    assert!(iter.next().is_none());

    for column in grid.columns_mut() {
        for c in column {
            *c += 1;
        }
    }
    assert_eq!(
        grid,
        Grid::new(vec![
            vec![1, 2, 3, 4],
            vec![5, 6, 7, 8],
            vec![9, 10, 11, 12]
        ])
        .unwrap()
    );
}

#[test]
fn grid_column_into_iter_mut() {
    let mut grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7], vec![8, 9, 10, 11]]).unwrap();
    let mut columns = grid.columns_mut();
    let column0 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column0.into_iter().map(|c| *c)),
        vec![0, 4, 8]
    );

    let column1 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column1.into_iter().map(|c| *c)),
        vec![1, 5, 9]
    );

    let column2 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column2.into_iter().map(|c| *c)),
        vec![2, 6, 10]
    );

    let column3 = columns.next().unwrap();
    assert_eq!(
        Vec::from_iter(column3.into_iter().map(|c| *c)),
        vec![3, 7, 11]
    );

    assert!(columns.next().is_none());

    assert_eq!(
        Vec::from_iter(grid.column_mut(1).into_iter().map(|c| *c).rev()),
        vec![9, 5, 1]
    );

    let column = grid.column_mut(1);
    let mut iter = column.into_iter().map(|c| *c);

    assert_eq!(iter.len(), 3);
    assert_eq!(iter.next().unwrap(), 1);

    let mut iter = iter.rev();
    assert_eq!(iter.len(), 2);
    assert_eq!(iter.next().unwrap(), 9);
    assert_eq!(iter.len(), 1);
    assert_eq!(iter.next().unwrap(), 5);
    assert_eq!(iter.len(), 0);
    assert!(iter.next().is_none());

    for column in grid.columns_mut() {
        for c in column {
            *c += 1;
        }
    }
    assert_eq!(
        grid,
        Grid::new(vec![
            vec![1, 2, 3, 4],
            vec![5, 6, 7, 8],
            vec![9, 10, 11, 12]
        ])
        .unwrap()
    );
}

#[test]
fn grid_indexing() {
    let grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7]]).unwrap();
    assert_eq!(grid[0][0], 0);
    assert_eq!(grid[0][2], 2);
    assert_eq!(grid[1][2], 6);
    assert_eq!(grid[1][3], 7);
}

#[test]
fn grid_cell_iter() {
    let mut grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7]]).unwrap();
    assert_eq!(Vec::from_iter(grid.cells().copied()), vec![0, 1, 2, 3, 4, 5, 6, 7]);
    assert_eq!(Vec::from_iter(grid.cells_mut().map(|c| *c)), vec![0, 1, 2, 3, 4, 5, 6, 7]);

    for c in grid.cells_mut() {
        *c += 1;
    }
    assert_eq!(Vec::from_iter(grid.cells().copied()), vec![1, 2, 3, 4, 5, 6, 7, 8]);
}

#[test]
fn grid_position() {
    let grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7]]).unwrap();

    assert_eq!(grid.position(|c| *c == 3), Some((0, 3)));
    assert_eq!(grid.position(|c| *c == 5), Some((1, 1)));
    assert_eq!(grid.position(|c| *c == 12), None);
}

#[test]
fn grid_mut_indexing() {
    let mut grid = Grid::new(vec![vec![0, 1, 2, 3], vec![4, 5, 6, 7]]).unwrap();
    grid[0][0] = 12;
    grid[0][2] = 13;
    grid[1][2] = 14;
    grid[1][3] = 15;

    assert_eq!(
        grid,
        Grid::new(vec![vec![12, 1, 13, 3], vec![4, 5, 14, 15]]).unwrap()
    );
}

#[cfg(test)]
#[derive(Debug, parse::prelude::HasParser)]
enum Entry {
    #[parse(string = "A")]
    A,
    #[parse(string = "B")]
    B,
}

#[cfg(test)]
impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::A => write!(f, "A"),
            Self::B => write!(f, "B"),
        }
    }
}

#[test]
fn grid_parse_display() {
    let grid: Grid<Entry> = parse::parse_str("AB\nBB\nAA\n").unwrap();
    assert_eq!(grid.to_string(), "AB\nBB\nAA\n");

    let grid: Grid<Entry> = Grid::new(vec![]).unwrap();
    assert_eq!(grid.to_string(), "<empty>\n");
}

#[test]
fn grid_parse_error() {
    parse::parse_str::<Grid<Entry>>("AB\nBB\nAAC\n").unwrap_err();
}

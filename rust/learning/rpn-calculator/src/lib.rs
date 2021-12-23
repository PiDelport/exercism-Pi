//! <https://exercism.org/tracks/rust/exercises/rpn-calculator>

#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

/// A binary operator that may fail.
type BinaryOperator = fn(i32, i32) -> Option<i32>;

/// RPN calculator
///
/// All methods signal failure using [`Option`].
#[derive(Default)]
struct RpnCalculator {
    stack: Vec<i32>,
}

impl RpnCalculator {
    fn new() -> Self {
        Self::default()
    }

    /// Execute a binary operator.
    fn operate(&mut self, op: BinaryOperator) -> Option<()> {
        let b = self.stack.pop()?;
        let a = self.stack.pop()?;
        let result = op(a, b)?;
        self.stack.push(result);
        Some(())
    }

    /// Evaluate one [`CalculatorInput`].
    fn evaluate_one(&mut self, input: &CalculatorInput) -> Option<()> {
        use CalculatorInput::*;
        match *input {
            Add => self.operate(i32::checked_add)?,
            Subtract => self.operate(i32::checked_sub)?,
            Multiply => self.operate(i32::checked_mul)?,
            Divide => self.operate(i32::checked_div)?,
            Value(n) => self.stack.push(n),
        }
        Some(())
    }

    /// Finish the computation, returning the final value, if any.
    fn finish(mut self) -> Option<i32> {
        if self.stack.len() == 1 {
            self.stack.pop()
        } else {
            None
        }
    }
}

/// Evaluate the given expression expression using [`RpnCalculator`].
pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut calculator = RpnCalculator::new();
    for input in inputs {
        calculator.evaluate_one(input)?;
    }
    calculator.finish()
}

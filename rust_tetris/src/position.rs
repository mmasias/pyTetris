/// Represents a 2D position with x and y coordinates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub x: i32,
    pub y: i32,
}

impl Position {
    /// Creates a new position with the given coordinates
    pub fn new(x: i32, y: i32) -> Self {
        Position { x, y }
    }

    /// Adds another position to this one, returning a new position
    #[allow(dead_code)]
    pub fn add(&self, other: &Position) -> Position {
        Position {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }

    /// Sets the x coordinate
    pub fn set_x(&mut self, x: i32) {
        self.x = x;
    }

    /// Sets the y coordinate
    pub fn set_y(&mut self, y: i32) {
        self.y = y;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_creation() {
        let pos = Position::new(5, 10);
        assert_eq!(pos.x, 5);
        assert_eq!(pos.y, 10);
    }

    #[test]
    fn test_position_add() {
        let pos1 = Position::new(1, 2);
        let pos2 = Position::new(3, 4);
        let result = pos1.add(&pos2);
        assert_eq!(result, Position::new(4, 6));
    }

    #[test]
    fn test_position_setters() {
        let mut pos = Position::new(0, 0);
        pos.set_x(10);
        pos.set_y(20);
        assert_eq!(pos.x, 10);
        assert_eq!(pos.y, 20);
    }
}
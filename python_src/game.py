"""Game entry point - main function to start Tetris game."""

from .tetris import Tetris


def main():
    """Main entry point for the Tetris game."""
    game = Tetris(10, 20)
    game.game_loop()


if __name__ == "__main__":
    main()
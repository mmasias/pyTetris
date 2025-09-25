public class Tetris {
    private Board board;
    private boolean gameRunning;
    private int score;
    private Console console;
    boolean[] bagPieceUsed={false,false,false,false,false,false,false};


    public Tetris(int width, int height) {
        board = new Board(width, height, new Console());
        gameRunning = true;
        score = 0;
        console = new Console();
    }

    public void spawnNewPiece() {
        Piece[] pieces = {
                PieceFactory.createIPiece(),
                PieceFactory.createOPiece(),
                PieceFactory.createTPiece(),
                PieceFactory.createLPiece(),
                PieceFactory.createJPiece(),
                PieceFactory.createSPiece(),
                PieceFactory.createZPiece()
        };
        
            int randomIndex = (int) (Math.random() * pieces.length);

            if(bagPieceUsed[0]&bagPieceUsed[1]&bagPieceUsed[2]&bagPieceUsed[3]&bagPieceUsed[4]&bagPieceUsed[5]&bagPieceUsed[6]){
                for (int k=0;k<bagPieceUsed.length;k++){
                    bagPieceUsed[k]=false;
                }
            }
            while(bagPieceUsed[randomIndex]){
                int newRandom =(int)(Math.random()*pieces.length);
                randomIndex=newRandom;
            }

            board.setCurrentPiece(pieces[randomIndex]);
            bagPieceUsed[randomIndex]=true;
            
            for (int i=0;i<bagPieceUsed.length;i++){
                if(bagPieceUsed[i]){
                    console.write("1 ");
                }else{
                    console.write("0 ");
                }
            }
            

    }

    private void processUserInput(String input) {
        if (input.length() != 0) {
            char command = input.charAt(0);

            switch (command) {
                case '4' -> {
                    if (board.canMovePiece(board.getCurrentPiece(), -1, 0))
                        board.getCurrentPiece().moveLeft();
                }
                case '6' -> {
                    if (board.canMovePiece(board.getCurrentPiece(), 1, 0))
                        board.getCurrentPiece().moveRight();
                }

                case '7' -> board.canRotatePiece(board.getCurrentPiece(), false);
                case '9' -> board.canRotatePiece(board.getCurrentPiece(), true);
            }
        }
    }

    public void gameLoop() {

        spawnNewPiece();
        while (gameRunning) {
            board.display();
            String input = console.readString("Score: "+score+"\n\nCommands:(4=left, 6=right, 7=counter↺, 9=clockwise↻): ");
            processUserInput(input);

            if (board.canMovePiece(board.getCurrentPiece(), 0, 1)) {
                board.getCurrentPiece().moveDown();
            } else {
                board.placePiece(board.getCurrentPiece());

                int linesCleared = board.clearCompleteLines();
                if (linesCleared>4){
                    linesCleared=4;
                }
                int[] linesClearedScore={0,100,300,500,800};
                String[] linesClearedName={"None","SINGLE","DOUBLE","TRIPLE","TETRIS!"};

                if (linesCleared>=1){
                    score+=linesClearedScore[linesCleared];  
                    console.writeln("¡" + linesCleared + " Lines cleared! "+linesClearedName[linesCleared]);
                    console.readString("Press Enter to continue...");
                }
                spawnNewPiece();

                if (!board.canMovePiece(board.getCurrentPiece(), 0, 0)) {
                    gameRunning = false;
                    console.writeln("¡GAME OVER!");
                    console.writeln("Final Score: " + score);
                }
            }
        }
    }
}
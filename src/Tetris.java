public class Tetris {
    private Board board;
    private boolean gameRunning;
    private int score;
    private Console console;

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
        
            boolean[] bagPieceUsed={false,false,false,false,false,false,false};
            int randomIndex = (int) (Math.random() * pieces.length);
            while(bagPieceUsed[randomIndex]=true){
                    int newRandom = (int) (Math.random() * pieces.length);
                    if(newRandom!=randomIndex){
                        randomIndex=newRandom;
                    }
                }
        bagPieceUsed[randomIndex]=true;
            for(int i=0,j=0; i<bagPieceUsed.length;i++){
                if(bagPieceUsed[i]=true){
                    j++;
                    System.out.println("Piece Count:"+j);
                }
                System.out.println("piece n."+i);
            if(j==bagPieceUsed.length){
                System.out.println("all used, new bag");
                for(int k=0; k<bagPieceUsed.length;k++){
                    bagPieceUsed[k]=false;
                }    
                }
            }
        board.setCurrentPiece(pieces[randomIndex]);
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
            String input = console.readString("Score: " + score + "\n\nComando (4=izq, 6=der, 7=rotar↺, 9=rotar↻): ");
            processUserInput(input);

            if (board.canMovePiece(board.getCurrentPiece(), 0, 1)) {
                board.getCurrentPiece().moveDown();
            } else {
                board.placePiece(board.getCurrentPiece());

                int linesCleared = board.clearCompleteLines();
                if (linesCleared==4){
                 score+=800;  
                }
                if (linesCleared==3){
                 score+=500;  
                }
                if (linesCleared==2){
                 score+=300;  
                }
                if (linesCleared==1){
                 score+=100;  
                }
                

                if (linesCleared==1) {
                    console.writeln("¡" + linesCleared + " línea eliminada! SINGLE");
                    console.readString("Presiona Enter para continuar...");
                }
                if (linesCleared==2) {
                    console.writeln("¡" + linesCleared + " líneas eliminadas! DOUBLE ");
                    console.readString("Presiona Enter para continuar...");
                }
                if (linesCleared==3) {
                    console.writeln("¡" + linesCleared + " líneas eliminadas! TRIPLE");
                    console.readString("Presiona Enter para continuar...");
                }
                if (linesCleared==4) {
                    console.writeln("¡" + linesCleared + " líneas eliminadas! TETRIS!");
                    console.readString("Presiona Enter para continuar...");
                }


                spawnNewPiece();

                if (!board.canMovePiece(board.getCurrentPiece(), 0, 0)) {
                    gameRunning = false;
                    console.writeln("¡GAME OVER!");
                    console.writeln("Puntuación final: " + score);
                }
            }
        }
    }
}
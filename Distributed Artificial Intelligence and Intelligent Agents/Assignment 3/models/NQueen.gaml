/**
* Name: NQueen
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model NQueen

global
{

    list<int> numbers <- [4,8,12];
    int N <- numbers[rnd(0,2)];   
    list<Queen> queens;
    list<ChessBoard> ChessBoards; 
    
    init
    {
    	//creating N numbers of queen
        create Queen number: N
        {
        	location <- ChessBoard[rnd(0,N-1),0].location;
        }        
    }
}

//creating NxN chess board
grid ChessBoard skills:[fipa] width:N height:N
{
	rgb color <- bool(((grid_x + grid_y) mod 2)) ? #black : #white;
	bool occupied <- false;
	init{
		add self to: ChessBoards;
   }
}

species Queen skills:[fipa]
{
    int queenIndex;
    int currentRow <- 0;
    ChessBoard selectedCell <-  nil;
    bool noPositionAvailable <- false;
    bool positionFound <- false;
    bool searchPosition <- false;
    
    init
    {
        add self to: queens;
        queenIndex <- int(queens[length(queens) -1]);
        if(length(queens) = N ){
            do start_conversation with:(to: list(queens[0]), protocol: 'fipa-request', performative: 'inform', contents: ['NextQueen']);        
            write "Start searching position!";
        }
    }
    
    aspect default
    {
       	draw cone3D(1.5,2.5) at: location color: #white ;
    	draw sphere(1) at: location + {0, 0, 2} color: #black ;
    }
     
     //inform predecesor to change position if no position is available
     reflex informPredecessor when: noPositionAvailable
     {
        do start_conversation with:(to: list(queens[queenIndex -1]), protocol: 'fipa-request', performative: 'inform', contents: ['NoPosition']);
        write name + ": Position not found, back to " + queens[queenIndex -1];
        noPositionAvailable <- false;
     }
     
     //inform next queen to start searching for position 
     reflex informNextQueen when: positionFound
     {
         if(queenIndex != N -1){
             write name + ": Position found";
             do start_conversation with:(to: list(queens[queenIndex +1]), protocol: 'fipa-request', performative: 'inform', contents: ['NextQueen']);
            
            noPositionAvailable <- false;
         }
         else
         {
             write "All positions found!";
         }
         positionFound <- false;        
     }
     
     //keep listning
     reflex listen_inform when: (!empty(informs))
     {
        message msgFromQueen <- (informs at 0);
        list<string> contents <- msgFromQueen.contents;
        if(contents[0] = 'NextQueen') //start searching for new position
        {
            searchPosition <- true;
            write name + ": Search for a new position";
        }
        else if(contents[0] = 'NoPosition') //ask to change position
        {
        	currentRow <- (currentRow +1) mod N;
        	positionFound <- false;
        	selectedCell.occupied <- false;
        	selectedCell <- nil;
              	
        	if(currentRow = 0){
        		noPositionAvailable <- true;
        	}
        	else
        	{
        		searchPosition <- true;        		
        	}            
        }
        informs <- nil;
     }
     
     int getListIndex(int curIndex, int curRow)
     {
     	return (N * curRow) + curIndex;
     }
     
     //search for a new position
     reflex searchPosition when: searchPosition
     {
        loop i from: currentRow to: N-1
        {   
        	if(ifRowColumnOccupied (i) = false and ifUpperDiagonalOccupied (i,queenIndex) = false and ifLowerDiagonalOccupied (i,queenIndex) = false)
        	{
        		if(selectedCell != nil)
        		{
        			selectedCell.occupied <- false;
        		}
        		currentRow <- i;
        		selectedCell <- ChessBoards[getListIndex(queenIndex, currentRow)];
        		location <- selectedCell.location;
        		selectedCell.occupied <- true;
        		ChessBoards[getListIndex(queenIndex, currentRow)] <- selectedCell;
        		searchPosition <- false;
        		positionFound <- true;
        		break;
        	}
        	
        	if(i = (N-1) and !positionFound)
        	{
        		noPositionAvailable <- true;
        		currentRow <- 0;
        		searchPosition <- false;
        		positionFound <- false;
        		break;
        	}
        }         
     }
     
     //check if row and column is occupied or not
     bool ifRowColumnOccupied (int curRow)
     {
     	int currentCol <- queenIndex -1;
     	if(currentCol >= 0)
     	{	
     		loop while: currentCol >= 0
     		{
        		ChessBoard tempCell <- ChessBoards[getListIndex(currentCol,curRow)];
        		if(tempCell.occupied = true)
        		{
        			return true;
        		}
        		currentCol <- currentCol -1;
       		} 
     	}
     	
        return false;
     }

     //check if upper diagonal is occupied or not
     bool ifUpperDiagonalOccupied (int curRow, int curIndex)
     {     	
     	int X <- curIndex;
     	int Y <- curRow;
     	
     	Y <- Y -1;
     	X <- X -1;
     	
     	loop while: (Y >= 0 and X >= 0)
     	{
			ChessBoard tempCell <- ChessBoards[getListIndex(X, Y)];
     		if(tempCell.occupied = true)
     		{
        		return true;
        	}
        	Y <- Y -1;
     		X <- X -1;
     	}
     	
     	return false;
     }
     
     //check if lower diagonal is occupied or not
     bool ifLowerDiagonalOccupied (int curRow, int curIndex)
     {
     	int X <- curIndex;
     	int Y <- curRow;
     	
     	Y <- Y +1;
     	X <- X -1;
     	
     	loop while: (Y < N and Y >= 0 and X >= 0)
     	{
			ChessBoard tempCell <- ChessBoards[getListIndex(X, Y)];
     		if(tempCell.occupied = true){
        		return true;
        	}
        	Y <- Y -1;
     		X <- X -1;
     	}
     	
     	return false;
     }
}

experiment main type: gui
{
    output
    {
        display map type: opengl
        {
            grid ChessBoard lines: #black ;
            species Queen;
        }
    }
}



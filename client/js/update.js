function blankBoard() {

  var iX = 20, iY = 10;
  var x = 0, y = 0;
  for(var i = 0; i < 8; i++){
    $("#devBoard").append("<br>");
    for(var j = 0; j < 8; j++){
      //draw canvas tile
      x = iX + j *50;
      y = iY + i *50;
      var id = String.fromCharCode(j+97)+(i+1);
      $("#devBoard").append("<canvas height=50 width=50 id='"+id+"' style='position:absolute;left:"+x+"px;top:"+y+"px;'></canvas>");
    }
  }    
}

function newBoard(){
  for(var i = 0; i < 8; i++){
    for(var j = 0; j < 8; j++){
      var id = String.fromCharCode(j+97)+(i+1);
      var pColor = 'green';
      
        var c = document.getElementById(id);
        var ctx = c.getContext("2d");
        ctx.beginPath();
        ctx.arc(25,25,23,0,2*Math.PI);
        ctx.fillStyle = pColor;
        ctx.strokeStyle = pColor;
        ctx.fill();
        ctx.stroke();
    }
  }
}

function updateBoardS(draw,board,moves,player){
var s = "";
var fix = 0;
  for(var i = 0; i < 8; i++){
    for(var j = 0; j < 8; j++){
      // update Board UI
      //draw pieces
      var id = String.fromCharCode(j+97)+(i+1);
      var pColor = "green";
      var c = document.getElementById(id);
      var ctx = c.getContext("2d");
      ctx.beginPath();
      if(board[i*8 + j] == "b"){
        pColor = "black";
      }
      if(board[i*8 + j] == "w"){
        pColor = "white";
      }

        ctx.arc(25,25,23,0,2*Math.PI);
        ctx.fillStyle = pColor;
        ctx.strokeStyle = pColor;
        ctx.fill();
        ctx.stroke();
      // update sBoard
      s += board[i*8 + j];
      //sBoard[i*8 + j] = board[i*8 + j];

      // add valid moves
      if(draw == "on" && moves.indexOf(id) != -1){
        if(player == 'b'){
          pColor = "black";
        }
        if(player == "w"){
          pColor = "white";
        }

        ctx.beginPath();
        ctx.arc(25,25,10,0,2*Math.PI);
        ctx.fillStyle = pColor;
        ctx.fill();
      }
    }
    fix +=1;
    if(i != 7){
      s += ","
    }
  }
  sBoard = s;
}


var sBoard = ["eeeeeeee",
                "eeeeeeee",
                "eeeeeeee",
                "eeewbeee",
                "eeebweee",
                "eeeeeeee",
                "eeeeeeee",
                "eeeeeeee"];
var player = "b";

function changePlayer(player){
  if(player == "b"){
    p = "w";
  }
  else if(player == "w"){
    p = "b";
  }
  return p;
}


  /*
function toJSON(board,move,player){
//string = "{\"board\":\"eeeeeeee,eeeeeeee,eeeeeeee,eeebweee,eeewbeee,eeeeeeee,eeeeeeee,eeeeeeee\",\"move\":\"tile65\",\"player\":\"w\"}"
  // add board
  s = "{\"board\":\"" + board;
  // add move
  s += "\",\"move\":\"" + move;
  // add player
  s += "\",\"player\":\"" + player + "\"}"
  return s;
}*/


function updateBoardTitle(player,error,count,end,id){
  
  if(end == true){
    if(count[0]>count[1]){
      gameEnd('b')
    }
    if(count[0]<count[1]){
      gameEnd('w')
    }
    //update count
    $("#pCount").text("Blacks: "+count[0]+", Whites: "+count[1]);
    return
  }else{

  }

  splayer = "";
  if(player == "b")
    splayer = "Black";
  if(player == "w")
    splayer = "White";
  $("#pBoardTitle").text(error+" Now, "+splayer+"'s turn!");
  //update count
  $("#pCount").text("Blacks: "+count[0]+", Whites: "+count[1] + " ");
}

function updateLogger(ini,player,move,board,moves){
  if(ini)
    $("#taLog").append("\n-------------------------------------------\nStart Game\n-------------------------------------------\n");
  //append Player
  if(!ini){
    splayer = "";
    if(player == "b")
      splayer = "\nWhite";
    if(player == "w")
      splayer = "\nBlack";
    $("#taLog").append("\n-------------------------------------------"+splayer+" play at ");
  }
  //append Move
  $("#taLog").append(move);
  //append Board
  //$("#pLog").append("Board:<br>"+makeBoard(board));
  $("#taLog").append("\n"+board);
  //append nextValidMoves
  $("#taLog").append("\nValid next moves:\n"+moves);
  
  $('#taLog').scrollTop($('#taLog')[0].scrollHeight);
}

function makeBoard(board){
  sBoard = "";
  for(i = 0; i < 64; i++){
    sBoard += board[i]+" ";
    if(i%8 == 7){
      sBoard += "<br>";
    }
  }
  return sBoard;
}

function gameEnd(w){
  if(w == 'b'){
    $("#pBoardTitle").text("Game End, Black Wins!");
  }
  if(w == 'w'){
    $("#pBoardTitle").text("Game End, White Wins!");
  }  
  if(w == 'd'){
    $("#pBoardTitle").text("Game End, Leads a Draw!");
  }
}

























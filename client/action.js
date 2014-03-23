  var currentData;

function action(){


  // Action event for button "New Game"
  $("#newGameButton").click(function(){
          //alert($(this).attr('id'));
          $.post("/game/new/0",
            function(data,status){
              
             //   if(status == success){
                    //change the board information
                    newBoard();
                    update(true,data);
           //     }
            });
          });
  $("#MCButton").click(function(){
          var simulation = document.getElementById('simulation').value;
          $.post("/game/aiplay/montecarlo/"+simulation,
            function(data,status){
              //if(data.isValid == "True"){
                //change the board information
                    update(false,data);
            });
          });
  $("#AIButton").click(function(){
    var evaluation = jQuery( 'input[name=evaluation]:checked' ).val();
    var search = jQuery('input[name=search]:checked').val();
    var depth = document.getElementById('depth').value;
          //alert($(this).attr('id'));
          $.post("/game/aiplay/"+evaluation+"/"+search+"/"+depth,
            function(data,status){
              //if(data.isValid == "True"){
                    
                //change the board information
                   update(false,data);
            });
          });
  $("canvas").click(function(){
          //alert($(this).attr('id'));
          $.post("/game/validmove/"+$(this).attr("id"),
            function(data,status){
              
                    update(false,data);
                //}
            });
          });
  $("canvas").mouseenter(function(){
    updateBoardS("on",currentData.board,currentData.moves,currentData.player);
  });
  $("canvas").mouseleave(function(){
    updateBoardS("off",currentData.board,currentData.moves,currentData.player);
  });

} 

function update(ini,data){
  currentData = data;
  updateBoardS("on",data.board,data.moves,data.player);
  //change the board title
  updateBoardTitle(data.player,data.serror,data.count,data.end);
  //update Game Logger
  updateLogger(ini,data.player,data.move,data.lboard,data.moves);
}


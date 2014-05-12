

function sign(){

	$("#signInButton").click(function(){
        // post 
        var pw = document.getElementById("pw").value
        var username = document.getElementById("username").value
        $.post("/signin/"+username+"/"+pw,
        function(data,status){
            
        	if(data.success==true){
        		// success, go to option page
                localStorage.setItem("user", username)
        		window.location = "option.html"   
        	}else{
                // fail, put in the information
                alert(data.error + " Please sign in again.")
            }
        })
        
    })

    $("#signUpButton").click(function(){
        // post 
        var email = document.getElementById("email").value
        var pw = document.getElementById("pw").value
        var username = document.getElementById("username").value
        $.post("/signup/"+email+"/"+pw+"/"+username,
        function(data,status){
        	if(data.success==true){
        		// success, go to option page
        		localStorage.setItem("user", username)
        		window.location = "option.html"	
        	}
            else{
                alert(data.error + " Please sing up again");
            }
        })
        
    })
}

function welcome () {
	var welcome = "Welcome! " + localStorage.getItem("user")
	$("#welcome").text(welcome)
}
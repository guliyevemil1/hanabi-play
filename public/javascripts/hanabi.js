var numToCardMap = { 1 : "One",  2 : "Two", 3 : "Three" , 4 : "Four", 5 : "Five" };

function sendAction(action) {
    var post_data = {
        type : "POST",
        url : document.URL,
        contentType : "application/json",
        data : JSON.stringify(action),
    };
    console.log(post_data);
    $.ajax(post_data);
}
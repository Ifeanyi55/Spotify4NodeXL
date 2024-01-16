// date-time code
function displayDateTime() {
    var date = new Date();
    var options = {
      weekday: 'long',
      year: 'numeric',
      month: 'long',
      day: 'numeric',
      hour: 'numeric',
      minute: 'numeric',
      second: 'numeric',
      hour12: true // display in 12-hour format
    };
    var dateTime = date.toLocaleString('en-US', options);
    document.getElementById("dateclock").innerHTML = dateTime;
  }
  
  displayDateTime();
  setInterval(displayDateTime, 1000); // update every second
    

// validate key press code
document
    .addEventListener("keydown",function(event){
    // if the user presses the "Enter" key on the keyboard
    if(event.key === "Enter"){
        // cancel the default action if any
        event.preventDefault();
        // trigger the button element with a click
        document.getElementById("validate").click();

    }
});


// info key press code
document
    .addEventListener("keydown",function(event){
    // if the user presses the "Enter" key on the keyboard
    if(event.ctrlKey && String.fromCharCode(event.keyCode).toLowerCase() === "i"){
        // cancel the default action if any
        event.preventDefault();
        // trigger the button element with a click
        document.getElementById("info").click();

    }
});

// code to print/save web page
const printPage = () => {
    window.print();
}

document
.getElementById("print")
.addEventListener("click",printPage)

// scroll to top

const scrollToTopButton = document.getElementById('scroll');

// Show the button when the user scrolls down 20px from the top of the document
window.onscroll = function() {
  if (document.documentElement.scrollTop > 20) {
    scrollToTopButton.style.display = 'block';
  } else {
    scrollToTopButton.style.display = 'none';
  }
};

// Scroll to the top of the document when the button is clicked
scrollToTopButton.addEventListener('click', function() {
  document.documentElement.scrollTo({
    top: 0,
    behavior: 'smooth'
  });
});

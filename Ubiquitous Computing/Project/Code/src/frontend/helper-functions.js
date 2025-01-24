$(() => {
  setInterval(cest, 500);

  cest();
});

function cest() {
  var now = new Date();
  var hours = now.getHours();
  var minutes = now.getMinutes();
  hours = firstZero(hours);
  minutes = firstZero(minutes);
  document.getElementById("cest").innerHTML = hours + ":" + minutes + " CEST";
}
function firstZero(num) {
  num = (num < 10 ? "0" : "") + num;
  return num;
}

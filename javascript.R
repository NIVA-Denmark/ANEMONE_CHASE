script <- "$('#QEtable tbody tr td:nth-last-child(2)').each(function() {
var cellValue = $(this).text().trim();
if (cellValue > 10) {
$(this).css('background-color', 'rgb(255,0,0)');
}
else if (cellValue > 5) {
$(this).css('background-color', 'rgb(255,140,43)');
}
else if (cellValue > 1) {
$(this).css('background-color', 'rgb(255,255,0)');
}
else if (cellValue > 0.5) {
$(this).css('background-color', 'rgb(0,214,0');
}
else if (cellValue > 0) {
$(this).css('background-color', 'rgb(0,126,255)');
}
});
$('#QEtable tbody tr td:last-child').each(function() {
var cellValue = $(this).text().trim();
if (cellValue == 'Bad') {
$(this).css('background-color', 'rgb(255,0,0)');
}
else if (cellValue == 'Poor') {
$(this).css('background-color', 'rgb(255,140,43)');
}
else if (cellValue == 'Moderate') {
$(this).css('background-color', 'rgb(255,255,0)');
}
else if (cellValue == 'Good') {
$(this).css('background-color', 'rgb(0,214,0');
}
else if (cellValue == 'High') {
$(this).css('background-color', 'rgb(0,126,255)');
}
});"


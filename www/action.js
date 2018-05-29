$(window).on('scroll', function(e) {
 if ($(window).scrollTop() > 300) {
    $("#banner").addClass("smaller");
  } else {
    if ($("#banner").hasClass("smaller")) {
      $("#banner").removeClass("smaller");
    }
  }
});

$(window).on('scroll', function(e) {
 if ($(window).scrollTop() > 500) {
    $("#banner2").addClass("smaller");
  } else {
    if ($("#banner2").hasClass("smaller")) {
      $("#banner2").removeClass("smaller");
    }
  }
});
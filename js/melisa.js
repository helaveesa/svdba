var activePhoto = "10";
var animationStack = [];
var animationComplete = true;
var base_url = '/';
var imageName = '';
var section = 'sect';

$( function init() {
  if (!$('#photoGallery').html()) {
    /* Image preload */
    var images_cache = [];
    for (var i = 0; i <= 10; i++) {
      images_cache[i] = new Image();
      images_cache[i].src = base_url+'img/photo-'+section+'-'+i+'.png';
    }

    /* Add image change handlers */
    $(".photo-options .options div").bind("click", function(e) {
      id = new String(this.id);
      id = id.substring(2);
      if (animationComplete != true) {
        return false;
      }
      changeImage(id);
      return false;
    });
  } else {
    $(".photo-options .options div").bind("click", function(e) {
      $.slimbox(galleryImages[$(this).attr('rel')], 0);
    });
  }
});

function changeImage(id) {
  animationComplete = false;
  el = document.getElementById('wp');
  var cover = $('#wp-cover');
  cover.css('width', 0);
  cover.css('backgroundImage', 'url('+base_url+'img/photo-'+section+'-'+id+'.png)');
  cover.animate({width: 617}, 800, function(e) {
    el.style.backgroundImage = 'url('+base_url+'img/photo-'+section+'-'+id+'.png)';
    animationComplete = true;
  });
}

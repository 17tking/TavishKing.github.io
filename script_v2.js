  
document.addEventListener('DOMContentLoaded', () => {
  const navbar = document.getElementById('navbar');
  // nav-bar color change effect
  if (navbar) {
    window.addEventListener('scroll', function () {
    console.log('scrolling!');
      if (window.scrollY > 1) {
        navbar.classList.add('scrolled');
      } else {
        navbar.classList.remove('scrolled');
      }
    });
  }


    // last name excel feature
  const name = document.querySelector('.hover-name');
  if (name) {
    name.addEventListener('click', () => {
      name.classList.add('cell-clicked');
      setTimeout(() => {
        name.classList.remove('cell-clicked');
      }, 4000);
    });
  }
});


// about me animation
const sections = document.querySelectorAll('.section');

const observer = new IntersectionObserver(entries => {
  entries.forEach(entry => {
    if (entry.isIntersecting) {
      entry.target.classList.add('visible');
      observer.unobserve(entry.target);
    }
  });
}, {
  threshold: 0.1
});

sections.forEach(section => {
  observer.observe(section);
});
function scrollToElement(elementSelector, instance = 0) {
    // Select all elements that match the given selector
    const elements = document.querySelectorAll(elementSelector);
    // Check if there are elements matching the selector and if the requested instance exists
    if (elements.length > instance) {
        // Scroll to the specified instance of the element
        elements[instance].scrollIntoView({ behavior: 'smooth' });
    }
}

const link1 = document.getElementById("link1");
const link2 = document.getElementById("link2");
const link3 = document.getElementById("link3");
const link4 = document.getElementById("link4");

link1.addEventListener('click', () => {
    scrollToElement('.header');
});

link2.addEventListener('click', () => {
    scrollToElement('.header', 1);
});

link3.addEventListener('click', () => {
    scrollToElement('.header', 2);
});

link4.addEventListener('click', () => {
    scrollToElement('.header', 3);
});


// EDA slideshow

const plotImages = [
  "R/plots/kpi1_10_highest.jpg",
  "R/plots/kpi1_10_lowest.jpg",
  "R/plots/kpi1_multiple_category_plot.jpg",
  "R/plots/kpi1_single_category_plot.jpg",
  "R/plots/kpi1_top_category_pairs.jpg",
  "R/plots/kpi1_heat_table_support_lift.png",
  "R/plots/kpi1_cat_popularity.jpg"
];

let currentIndex = 0;

function updatePlot() {
  const img = document.getElementById("edaPlot");
  const prevBtn = document.getElementById("prevBtn");
  const nextBtn = document.getElementById("nextBtn");

  img.src = plotImages[currentIndex];

  // Disable buttons when at start or end
  prevBtn.disabled = currentIndex === 0;
  nextBtn.disabled = currentIndex === plotImages.length - 1;
}

function changePlot(direction) {
  currentIndex += direction;
  updatePlot();
}

// Initialize
updatePlot();

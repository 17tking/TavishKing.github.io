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


// EDA slideshows
const slideshows = {
  1: {
    images: [
      "R/plots/kpi1_10_highest.jpg",
      "R/plots/kpi1_10_lowest.jpg",
      "R/plots/kpi1_multiple_category_plot.jpg",
      "R/plots/kpi1_cat_popularity.jpg",
      "R/plots/kpi1_top_category_pairs.jpg",
      "R/plots/kpi1_heat_table_support_lift.jpeg"
    ],
    index: 0
  },
  2: {
    images: [
      "R/plots/kpi2_10_highest_reviewed.jpg",
      "R/plots/kpi2_10_lowest_reviewed.jpg",
      "R/plots/kpi2_review_dist.jpg",
      "R/plots/kpi2_size_v_score.jpg",
      "R/plots/kpi2_delivery_v_score.jpg",
      "R/plots/kpi2_price_v_score.jpg",
      "R/plots/kpi2_payment_v_score.jpg"
    ],
    index: 0
  },
  5: {
    images: [
      "Images/LOGISTICS.jpg"
    ],
    index: 0
  },
   6: {
    images: [
      "R/plots/kpi2_OR_ordersize.jpg",
      "R/plots/kpi2_OR_deliverydays.jpg",
      "R/plots/kpi2_OR_totprice.jpg",
      "R/plots/kpi2_OR_paymentvalue.jpg"
    ],
    index: 0
  },
  7: {
    images: [
      "R/plots/kpi3_brazil_sales_map.jpg",
      "R/plots/kpi3_brazil_orders_map.jpg",
      "R/plots/kpi3_brazil_aov_map.jpg",
      "R/plots/kpi3_brazil_delivery_map.jpg"
    ],
    index: 0
  }
};

function updatePlot(kpi) {
  const slideshow = slideshows[kpi];
  const img = document.getElementById(`edaPlot${kpi}`);
  const prevBtn = document.getElementById(`prevBtn${kpi}`);
  const nextBtn = document.getElementById(`nextBtn${kpi}`);

  img.src = slideshow.images[slideshow.index];
  prevBtn.disabled = slideshow.index === 0;
  nextBtn.disabled = slideshow.index === slideshow.images.length - 1;
}

function changePlot(kpi, direction) {
  slideshows[kpi].index += direction;
  updatePlot(kpi);
}

// Initialize all
updatePlot(1);
updatePlot(2);

updatePlot(4);
updatePlot(5);


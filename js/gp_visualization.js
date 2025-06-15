
document.addEventListener('DOMContentLoaded', function() {
  const container = d3.select("#gp-visualization");
  
  const containerWidth = container.node().getBoundingClientRect().width;
  const containerHeight = Math.min(containerWidth * 0.4, 400); // Aspect ratio control
  const margin = {top: 20, right: 0, bottom: 40, left: 0};

  const svg = d3.select("#gp-visualization")
    .append("svg")
    .attr("width", "100%")
    .attr("height", containerHeight)
    .attr("viewBox", `0 0 ${containerWidth} ${containerHeight}`)
    .attr("preserveAspectRatio", "xMidYMid meet");

  let g = svg.append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`)
  
  // GP parameters
  const l = 1.0;
  const sigma_f = 1.0;
  const sigma_n = 0.25;
  const n_points = 15;
  
  const xScale = d3.scaleLinear()
    .domain([-7.5, 7.5])
    .range([0, containerWidth - margin.left - margin.right]);
  
  const yScale = d3.scaleLinear()
    .domain([-2, 2])
    .range([containerHeight - margin.top - margin.bottom, 0]);
  
  function updateGP() {
    try {
      // Generate data
      const X_train = Array.from({length: n_points}, () => 15 * (Math.random() - 0.5));
      const y_train = X_train.map(x => Math.sin(x) + sigma_n * (Math.random() * 2 - 1));
      
      // Test points
      const X_test = Array.from({length: 100}, (_, i) => -7.5 + i * 15 / 99);
      
      // Simplified GP calculation
      const predictions = X_test.map(x_test => {
        const weights = X_train.map(x_train => 
          sigma_f * Math.exp(-0.5 * Math.pow(x_test - x_train, 2) / Math.pow(l, 2)));
        
        const totalWeight = weights.reduce((a, b) => a + b, 0);
        let mean = 0;
        let variance = sigma_f;
        
        if (totalWeight > 0) {
          mean = weights.reduce((sum, w, i) => sum + w * y_train[i], 0) / totalWeight;
          variance = sigma_f - Math.min(0.9 * sigma_f, 
                    weights.reduce((sum, w) => sum + w * w, 0) / totalWeight);
        }
        
        return {
          x: x_test, 
          mean, 
          variance,
          lower: mean - 2 * Math.sqrt(variance),
          upper: mean + 2 * Math.sqrt(variance)
        };
      });
      
      // Create a new group for the new visualization with initial opacity of 0
      svg.selectAll("g").each(function(d, i) {
        if (this !== g.node()) {
          // Remove any stale groups that weren't properly cleaned up
          d3.select(this).remove();
        }
      });

      const oldG = g; // Keep reference to the old group for transition

      const newG = svg.append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`)
        .style("opacity", 0);

      g = newG; // Update reference to the new group
      
      // Plot confidence interval on the new group
      const area = d3.area()
        .x(d => xScale(d.x))
        .y0(d => yScale(d.lower))
        .y1(d => yScale(d.upper))
        .curve(d3.curveBasis);
      
      newG.append("path")
        .datum(predictions)
        .attr("fill", "#F7F7F7") // gray98 equivalent
        .attr("d", area);
      
      // Generate sample paths from posterior
      const numPaths = 5;
      const pathLine = d3.line()
        .x(d => xScale(d.x))
        .y(d => yScale(d.y))
        .curve(d3.curveBasis);

      for (let p = 0; p < numPaths; p++) {
        // Use Cholesky-like approach for multivariate normal sampling
        const z = Array(X_test.length).fill(0).map(() => Math.random() * 2 - 1);
        
        // Apply covariance structure
        const sampleY = [];
        const mult = 0.4;  // this is how we can control the amplitude of pp preds
        for (let i = 0; i < X_test.length; i++) {
          let y = predictions[i].mean;
          
          // Apply correlation structure based on kernel
          for (let j = 0; j < X_test.length; j++) {
            if (Math.abs(i-j) < 20) { // Limit correlation window for performance
              const dist = Math.abs(X_test[i] - X_test[j]);
              const corr = Math.exp(-0.5 * Math.pow(dist / l, 2));
              y += mult * corr * z[j] * Math.sqrt(predictions[j].variance);
            }
          }
          sampleY.push(y);
        }
        
        const samplePath = X_test.map((x, idx) => ({
          x: x,
          y: sampleY[idx]
        }));

        newG.append("path")
          .datum(samplePath)
          .attr("fill", "none")
          .attr("stroke", "#75374240")
          .attr("stroke-width", 1.5)
          .attr("d", pathLine);
      }
    
      // Plot mean function
      const line = d3.line()
        .x(d => xScale(d.x))
        .y(d => yScale(d.mean))
        .curve(d3.curveBasis);
      
      newG.append("path")
        .datum(predictions)
        .attr("fill", "none")
        .attr("stroke", "#BFCDE0") 
        .attr("stroke-width", 2)
        .attr("d", line);
      
      // Plot training points
      newG.selectAll(".point")
        .data(X_train.map((x, i) => ({ x, y: y_train[i] })))
        .enter()
        .append("circle")
        .attr("class", "point")
        .attr("cx", d => xScale(d.x))
        .attr("cy", d => yScale(d.y))
        .attr("r", 4)
        .attr("fill", "#084887") 
        .attr("opacity", 0.7);
      
      // Crossfade between old and new visualizations
      newG.transition()
        .duration(3000)
        .style("opacity", 1);

      oldG.transition()
        .duration(3000)
        .style("opacity", 0)
        .on("end", function() {
          // Ensure complete removal
          d3.select(this).remove();
        });

      // Update reference AFTER both transitions have been set up
      setTimeout(() => {
        g = newG;
      }, 200);
      
    } catch (err) {
      console.error("GP calculation error:", err);
    }
  }
  
  setTimeout(() => {
    updateGP();
    setInterval(updateGP, 3000);
  }, 100);
});
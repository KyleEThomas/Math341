#R code to compare population data (when you know entire population)
#to sampling distribution and bootstrap distribution

#This code loads or installs mosaic:
{
  #load the mosaic library
  #require returns true if the library loads
  if(!require("mosaic")){
    #if it didn't load we can try to install it
    install.packages("mosaic")
    if(!require("mosaic")){
      stop("Mosaic isn't loading for some reason! :(")
    }
  }
  #mosaic installs this too we will use it for grid.arrange to print two plots together.
  require("gridExtra")
  }

# R and mosaic we are going to use:
#  gf_dotplot builds a dotplot plot
#  gf_histogram builds a histogram plot
#  gf_labs sets labels on a plot
#  gf_refine lets us refine other things like the x axis
#  sample(x, size) picks size things out of x WITHOUT replacement
#  resample(x, size) picks size things out of x WITH replacement
#  do(n) builds a special object that let you do what ever you * it with many times.
#    Some R coding magic that I don't really understand, but it makes things easy - Chris


# Running this code - what you need to know: we are going to make a histogram.
# Later we will explore adding adding 2.5%, 50% and 97.5% percentile lines, mean and 2 sd lines
# Finally add a normal curve based on the mean and sd
{
  #3 color blind safe colors on the viridis color scale
  colors = viridis::viridis(3)
  #The arguments should be self explanatory see Chris during office hour for a detailed explanation.
  buildHistogram <- function(data, gformula, title = "",
      addQuantLines = FALSE, addSDLines = FALSE,
      addNormalCurve = FALSE,
      limits = NULL, label = TRUE){
    binwidth <- 0.1
    histogram <- 
      gf_histogram(
        data = data, gformula,
        binwidth = binwidth
      ) %>%
      gf_labs(
        title = title,
        y = NULL
      )
    if(label){
      histogram <-
        histogram + scale_x_continuous(
          minor_breaks = seq(0, 200, 0.5), 
          breaks = seq(0, 200, 1),
          limits = limits
        )
    }else{
      histogram <- histogram +scale_x_continuous(limits = limits)
    }
    if(addQuantLines){
      histogram <-
        histogram +
        geom_vline(
          data = data.frame(
            quantLines <- quantile(
              data = data, gformula,
              probs = c(0.025,0.50,0.975)
            )
          ),
          aes(xintercept = quantLines, color = "quant"),
          lwd = 1.5,
          lty = c(3,1,3)
        )
    }
    if(addSDLines){
      SDLines <- mean(data = data, gformula) +
        c(-2,0,2) * sd(data = data, gformula)
      histogram <-
        histogram +
        geom_vline(
          data = data.frame(
            SDLines <- mean(data = data, gformula) +
              c(-2,0,2) * sd(data = data, gformula)
          ),
          aes(xintercept = SDLines, color = "SD"),
          lwd = 1.5,
          lty = c(3,1,3)
        )
    }
    if(addNormalCurve){
      histogram <- 
        histogram +
        stat_function(
          fun = function(x)
            dnorm(x, mean(data = data, gformula), sd(data = data, gformula)) *
            binwidth * nrow(data),
          lwd = 1.5,
          aes(colour = "Curve"),
          show.legend = ifelse(addQuantLines || addSDLines, FALSE, NA)
        )
    }
    histogram <-
      histogram + 
      scale_colour_manual(
        name = "Legend",
        values = c("quant" = colors[1],
                   "SD" = colors[2],
                   "Curve" = colors[3]),
        labels = c("quant" = "2.5, 50, and 97.5\nPercentile",
                   "SD" = "Mean and SD",
                   "Curve" = "Normal")
      )
    if(!label)histogram <- histogram + theme(legend.position = "none")
    histogram
  }
}

#First look at the entire population (which isn't usually known)
#Note: If data from the whole population is known, you don't need estimates

#Here we are going to use weights in Mosaic's Gestation dataset
#If we want information about it you can run:
{
  help(Gestation)
}


#Create a dot plot + histogram of the population data set
#This allows you to visualize the population data
{
  #if we store the value of gf_dotplot it doesn't plot right away
  dotplot <- gf_dotplot(data = Gestation, ~ wt, binwidth = 1, dotsize = 1) %>%
    gf_labs(
      title = "Population Data: Baby Boy Birth Weights - Dot Plot"
    ) %>%
    gf_refine(
      scale_x_continuous(
        minor_breaks = seq(0, 200, 1), 
        breaks = seq(0, 200, 5)
      )
    )
  #if we store the value of gf_histogram it also doesn't plot right away
  histogram <- gf_histogram(data = Gestation, ~ wt, binwidth = 1) %>%
    gf_labs(
      title = "Population Data: Baby Boy Birth Weights - Histogram"
    ) %>%
    gf_refine(
      scale_x_continuous(
        minor_breaks = seq(0, 200, 1), 
        breaks = seq(0, 200, 5)
      )
    )
  #we can pass them to grid.arrange to plot them together
  grid.arrange(dotplot, histogram, nrow=2)
}
#Note gf_dotplot and gf_histogram work almost exactly the same, just dotplot
#  is better suited to small populations/samples.
#dotsize = 1 is the default set it to dotsize = 0.75 to see all the dots


#Create a sampling distribution
#Randomly select samples of size n<length(population); do this 1000 times
#Find mean each time; call these 1000 sample means sampdist
#First check size of population and then choose n<pop size

#Our sample can't be bigger then our population because how can you select more
#  things then then you are picking from?

{
  n <- 20
  sampdist <- do(1000) * mean(data = sample(Gestation, size = n), ~ wt)
}

#Now if you look at the R Studio Environment Window sampdist should be 1000 
#  observations of 1 variable

#Create a Histogram to illustrate the distribution of the 1000 sample means
#This allows you to visualize the sampling distribution (of sample means)
{
  sampleHistogram <- buildHistogram(
    data = sampdist, ~ mean, title = "Sample Means - Histogram",
    addQuantLines = FALSE, addSDLines = FALSE
  )
  print(sampleHistogram)
  
  print(quantile(data = sampdist, ~ mean, probs = c(0.025,0.50,0.975)))
  cat("mean:\n")
  print(mean(data = sampdist, ~ mean))
  cat("SD:\n")
  print(sd(data = sampdist, ~ mean))
}
#Set addQuantLines = TRUE to add quantile/percentile lines
#Set addSDLines = TRUE to add Mean and SD lines

#Usually don't have entire population; use bootstrap technique
#Need a sample randomly chosen from population
#When you don't have population data, this sample is what you start with
#Here we have the population, so randomly select that sample of size k
#Need k data points from the population - sample without replacement
{
  k <- 20
  samplePop <- sample(Gestation, size = k)
}


#Randomly select, with replacement, a sample of size k from "samplePop"
#Do this 1000 times save the mean each time
#These 1000 sample means are the bootstrap distribution
bootstrap <- do(1000) * mean(data = resample(samplePop, size = k), ~ wt)

#Now if you look at the R Studio Environment Window bootstrap should be 1000 
#  observations of 1 variable

#Create a histogram of the bootstrap distribution (1000 sample means)
#This allows you to visualize the bootstrap distribution
{
  bootstrapHistogram <- buildHistogram(
    data = bootstrap, ~ mean, title = "Bootstrap means - Histogram",
    addQuantLines = FALSE, addSDLines = FALSE
  )
  print(bootstrapHistogram)
  
  print(quantile(data = bootstrap, ~ mean, probs = c(0.025,0.50,0.975)))
  cat("mean:\n")
  print(mean(data = bootstrap, ~ mean))
  cat("SD:\n")
  print(sd(data = bootstrap, ~ mean))
}
#Set addQuantLines = TRUE to add quantile/percentile lines
#Set addSDLines = TRUE to add Mean and SD lines

#Now let's compare the bootstrap with the sampling distribution
{
  sampleHistogram <- buildHistogram(
    data = sampdist, ~ mean, title = "Sample Means - Histogram", addQuantLines = TRUE, addSDLines = TRUE
  )
  bootstrapHistogram <- buildHistogram(
    data = bootstrap, ~ mean, title = "Bootstrap means - Histogram", addQuantLines = TRUE, addSDLines = TRUE
  )
  grid.arrange(sampleHistogram, bootstrapHistogram, nrow=2)
}

#There is a good chance they didn't line up because the min and max of the two sets isn't the same
#So here we get the min/max of both and use those to build our histogram limits
{
  limits <- c(
    floor(min(sampdist$mean, bootstrap$mean)),
    ceiling(max(sampdist$mean, bootstrap$mean))
  )
  sampleHistogram <- buildHistogram(
    data = sampdist, ~ mean, title = "Sample Means - Histogram",
    addQuantLines = TRUE, addSDLines = TRUE,
    addNormalCurve = FALSE, #set to TRUE add Normal Curve
    limits = limits
  )
  bootstrapHistogram <- buildHistogram(
    data = bootstrap, ~ mean, title = "Bootstrap means - Histogram",
    addQuantLines = TRUE, addSDLines = TRUE,
    addNormalCurve = FALSE, #set to TRUE add Normal Curve
    limits = limits
  )
  
  suppressWarnings(grid.arrange(sampleHistogram, bootstrapHistogram, nrow=2))
}


#Question: While the data may be close what factors may make it not line up?
#Hint:
{
  limit <- c(min(~ wt, data = Gestation), max(~ wt, data = Gestation))
  suppressWarnings(grid.arrange(
    gf_violin(""~wt, data = Gestation) +
      geom_boxplot(width=0.5, color="darkgrey") +
      labs(title = "Population") +
      xlim(limit[1], limit[2]),
    gf_violin(""~wt, data = samplePop) +
      geom_boxplot(width=0.5, color="darkgrey") +
      labs(title = "Sample") +
      xlim(limit[1], limit[2]),
    gf_violin(""~mean, data = sampdist) +
      geom_boxplot(width=0.5, color="darkgrey") +
      labs(title = "Sample Means") +
      xlim(limit[1], limit[2]),
    gf_violin(""~mean, data = bootstrap) +
      geom_boxplot(width=0.5, color="darkgrey") +
      labs(title = "Bootstrap Means") +
      xlim(limit[1], limit[2]),
    buildHistogram(
      data = sampdist, ~ mean, title = "Sample Means - Histogram",
      addQuantLines = TRUE, addSDLines = TRUE, addNormalCurve = TRUE,
      limits = limit, label = FALSE),
    buildHistogram(
      data = bootstrap, ~ mean, title = "Bootstrap means - Histogram",
      addQuantLines = TRUE, addSDLines = TRUE, addNormalCurve = TRUE,
      limits = limit, label = FALSE),
    nrow=6
  ))
}

#Hit "Source" for this script code and let it run till you get to the end.
#  How does the Sampling Distribution mean and 2*sd lines behave?
#  - Are they semi constant?  Or do they jump around?
#  - How about the lines for the Bootstrap Distribution?

#This helps grow sampdist and bootstrap, as they grow they become more like a normal
{
  newSampleSize = 1000# + 1000# + 2000# + 4000# + 8000# + 16000# + 18000
  if(newSampleSize > nrow(sampdist)){
    sampdist <- rbind(
      sampdist,
      do(newSampleSize - nrow(sampdist)) *
        mean(data = sample(Gestation, size = n), ~ wt)
    )
  }
  if(newSampleSize > nrow(bootstrap)){
    bootstrap <- rbind(
      bootstrap,
      do(newSampleSize - nrow(bootstrap)) *
        mean(data = resample(samplePop, size = k), ~ wt)
    )
  }

  limits <- c(
    floor(min(sampdist$mean, bootstrap$mean)),
    ceiling(max(sampdist$mean, bootstrap$mean))
  )
  sampleHistogram <- buildHistogram(
    data = sampdist, ~ mean, title = "Sample Means - Histogram",
    addQuantLines = TRUE, addSDLines = TRUE,
    addNormalCurve = TRUE, #set to TRUE add Normal Curve
    limits = limits
  )
  bootstrapHistogram <- buildHistogram(
    data = bootstrap, ~ mean, title = "Bootstrap means - Histogram",
    addQuantLines = TRUE, addSDLines = TRUE,
    addNormalCurve = TRUE, #set to TRUE add Normal Curve
    limits = limits
  )
  
  suppressWarnings(grid.arrange(sampleHistogram, bootstrapHistogram, nrow=2))
}


#Code for Bootstrap Distribution

#require returns true if the library loads
if(!require("mosaic")){
  #if it didn't load we can try to install it
  install.packages("mosaic")
  if(!require("mosaic")){
    stop("Mosaic isn't loading for some reason! :(")
  }
}

# Running this code - what you need to know: we are going to make a histogram.
{
  # The arguments should be self explanatory see Chris during office hour for a
  #   detailed explanation.
  buildHistogram <- function(data, gformula, title = "", binwidth = 1/8){
    data %>%
    gf_histogram(
      gformula,
      binwidth = binwidth
    ) %>%
    gf_labs(
      title = title,
      y = NULL
    ) %>% gf_refine(
      scale_x_continuous(
        minor_breaks = seq(0, 200, 0.5),
        breaks = seq(0, 200, 1),
        limits = NULL
      )
    )
  }
}

onesamp<-c(43, 59, 22, 25, 36, 47, 19, 21)
{
  bootstrap <- do(5000) * mean(sample(onesamp,8,TRUE))
}

bootmean<-mean(bootstrap$mean)
bootsd<-sd(bootstrap$mean)

cat("bootmean:", bootmean, "\n")
cat("bootsd:", bootsd, "\n")

bootlower<-quantile(bootstrap$mean,0.025)
bootupper<-quantile(bootstrap$mean,0.975)

cat("bootlower:", bootlower, "\n")
cat("bootupper:", bootupper, "\n")


#This allows you to visualize the bootstrap distribution
{
  bootstrapHistogram <- buildHistogram(
    data = bootstrap, ~ mean, title = "Bootstrap means - Histogram"
  )
  print(bootstrapHistogram)
}

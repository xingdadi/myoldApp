shinyServer(function(input, output) {

  output$main_plot <- renderPlot({

    data = read.table(file = "PM_data.txt", sep = "", header = FALSE);

    x = as.numeric( as.matrix(data[2, ]) );  #longitude
    y = as.numeric( as.matrix(data[1,]) );   #latitude
    time_lable = c("5am", "6am", "7am", "8am", "9am", "10am", "11am", "12 noon", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "10pm", "11pm", "0am midnight")

    tp = 1;   #1-18
    z = as.numeric( as.matrix(data[tp+2, ]) );  #PM2.5
    data_use = cbind(x, y, z) ;
    coords <- as.matrix(cbind(x, y));

    x.res <- 20
    y.res <- 20
    idx = which(z != "NA");
    surf <- mba.surf(data_use[idx, ], no.X = x.res, no.Y = y.res, h = 5, m = 2, extend = FALSE)$xyz.est;

    #--------zip code prediction
    zipcode_data = read.table(file = "zipcode.txt", header = TRUE, sep = "");
    zipcode_info = zipcode_data$zipcode
    longitude_info = zipcode_data$longitude
    latitude_info = zipcode_data$latitude

    code_now = as.numeric(input$n_zipcode);          #-------input the zipcode
    idx_now = which(zipcode_info == code_now);
    longitude_now = longitude_info[idx_now];
    latitude_now = latitude_info[idx_now];

    long_idx = which.min( abs(surf$x-longitude_now) );
    latd_idx = which.min( abs(surf$y-latitude_now) );

    pm_pred_now = surf$z[long_idx, latd_idx];

    #------zipcode pred plot
    main_label = paste("PM2.5 at", time_lable[tp]);
    text_label = paste(as.character(round(pm_pred_now, 1)),"(", as.character(code_now), ")", sep ="");
    image.plot(surf, xaxs = "r", yaxs = "r", main = main_label, xlab = "longitude", ylab = "latitude", xlim = c(min(x), max(x)), ylim = c(min(y), max(y)));
    points(longitude_now, latitude_now, pch = 2, cex = sqrt(pm_pred_now)/10)
    text(longitude_now+0.04, latitude_now+0.04, label = text_label)

    if (input$individual_obs) {
      points(x, y, pch = 19);
    }

    if (input$whole_area) {

      x.res_new <- input$rs_adjust
      y.res_new <- input$rs_adjust     
      surf_new <- mba.surf(data_use[idx, ], no.X = x.res_new, no.Y = y.res_new, h = 5, m = 2, extend = FALSE)$xyz.est;

      long_idx_new = which.min( abs(surf_new$x-longitude_now) );
      latd_idx_new = which.min( abs(surf_new$y-latitude_now) );

      pm_pred_new = surf_new$z[long_idx_new, latd_idx_new];

      text_label_new = paste(as.character(round(pm_pred_new, 1)),"(", as.character(code_now), ")", sep ="");
      image.plot(surf_new, xaxs = "r", yaxs = "r", main = main_label, xlab = "longitude", ylab = "latitude");
      points(longitude_now, latitude_now, pch = 2, cex = sqrt(pm_pred_new)/10);
      text(longitude_now+0.04, latitude_now+0.04, label = text_label_new);
      
    }

  })
})


#       
#    ## logfile
#    cat(paste("\nShortwave SW_in < -5 W/m2:",length(sw_bad_in),"\n"))
#    #cat(paste(db.bamet$UTC[sw_bad_in] ,db.bamet$SR01Up_Avg[sw_bad_in],"\n"))
#    
#    cat(paste("Shortwave SW_out < -5 W/m2:",length(sw_bad_out),"\n"))
#    #cat(paste(db.bamet$UTC[sw_bad_out],db.bamet$SR01Dn_Avg[sw_bad_out],"\n"))
#    
#    cat(paste("Shortwave SW_in < SW_out:",length(sw_weird),"\n\n"))
#    #cat(paste(db.bamet$UTC[sw_weird],(db.bamet$SR01Up_Avg-db.bamet$SR01Dn_Avg)[sw_weird],"\n"))
#    
#    cat(paste("Longwave LW_in  < 100 W/m2 and LW_in  > 500 W/m2:",length(lw_bad_in),"\n"))
#    #cat(paste(db.bamet$UTC[lw_bad_in] ,db.bamet$IR01UpCo_Avg[lw_bad_in],"\n"))
#    
#    cat(paste("Longwave LW_out < 100 W/m2 and LW_out > 500 W/m2:",length(lw_bad_out),"\n"))
#    #cat(paste(db.bamet$UTC[lw_bad_out],db.bamet$IR01DnCo_Avg[lw_bad_out],"\n"))
#    
#    cat(paste("Longwave (LW_in - LW_out)>20 W/m2:",length(lw_bad_diff),"\n"))
#    cat(paste(db.bamet$UTC[lw_bad_diff],(db.bamet$IR01UpCo_Avg-db.bamet$IR01DnCo_Avg)[lw_bad_diff],"\n"))
#    
#    cat(paste("Albedo > 1:",length(albedo_bad),"\n"))
#    #cat(paste(db.bamet$UTC[albedo_bad] ,db.bamet$Albedo[albedo_bad],"\n"))
#    
#    cat(paste("\nsnow covered sensors:",length(snow),"\n"))
#    cat(paste(db.bamet$UTC[snow],db.bamet$Albedo[snow],(db.bamet$SR01Up_Avg-db.bamet$SR01Dn_Avg)[snow],"\n"))
#    
#    # printing some stats
#    # -------------------
#      cat(paste0("\ntotal possible records: ",length(db.bamet$UTC),"\n\n              SW_in    SW_out    LW_in    LW_out\n---------------------------------------------------------------\n # records    ",
#                length(db.bamet.lvl1$SR01Up_Avg[!is.na(db.bamet.lvl1$SR01Up_Avg)])  ,"    ",                                  # records per sensor
#                length(db.bamet.lvl1$SR01Dn_Avg[!is.na(db.bamet.lvl1$SR01Dn_Avg)])  ,"     ",                                 # records per sensor
#                length(db.bamet.lvl1$IR01UpCo_Avg[!is.na(db.bamet.lvl1$IR01UpCo_Avg)]),"    ",                                # records per sensor
#                length(db.bamet.lvl1$IR01DnCo_Avg[!is.na(db.bamet.lvl1$IR01DnCo_Avg)]),"\n flag=0       ",                    # records per sensor
#                
#                format(length(db.bamet.lvl1$SR01Up_Avg[sw_in])/length(db.bamet$UTC)*100,nsmall=2 ),"%   ",                            # flag=0 records [%]
#                format(length(db.bamet.lvl1$SR01Dn_Avg[sw_in])/length(db.bamet$UTC)*100,nsmall=2 ),"%    ",                           # flag=0 records [%]
#                format(length(db.bamet.lvl1$IR01UpCo_Avg[sw_in])/length(db.bamet$UTC)*100,nsmall=2 ),"%   ",                          # flag=0 records [%]
#                format(length(db.bamet.lvl1$IR01DnCo_Avg[sw_in])/length(db.bamet$UTC)*100,nsmall=2 ),"%   (with NA)\n flag=0       ", # flag=0 records [%]
#                
#                format(length(db.bamet.lvl1$SR01Up_Avg[sw_in])/length(db.bamet.lvl1$SR01Up_Avg[!is.na(db.bamet.lvl1$SR01Up_Avg)])*100,nsmall=2 ),"%   ",         # flag=0 records [%] no NA
#                format(length(db.bamet.lvl1$SR01Dn_Avg[sw_in])/length(db.bamet.lvl1$SR01Dn_Avg[!is.na(db.bamet.lvl1$SR01Dn_Avg)])*100,nsmall=2 ),"%    ",        # flag=0 records [%] no NA
#                format(length(db.bamet.lvl1$IR01UpCo_Avg[sw_in])/length(db.bamet.lvl1$IR01UpCo_Avg[!is.na(db.bamet.lvl1$IR01Up_Avg)])*100,nsmall=2 ),"%   ",     # flag=0 records [%] no NA
#                format(length(db.bamet.lvl1$IR01DnCo_Avg[sw_in])/length(db.bamet.lvl1$IR01DnCo_Avg[!is.na(db.bamet.lvl1$IR01DnCo_Avg)])*100,nsmall=2 ),"%   (without NA) \n flag=12       ", # flag=0 records [%] no NA
#                
#                format(length(db.bamet.lvl1$SR01Up_Avg[sw_weird])/length(db.bamet.lvl1$SR01Up_Avg[!is.na(db.bamet.lvl1$SR01Up_Avg)])*100,nsmall=2 ),"%    ",           # flag=12 records [%] no NA
#                format(length(db.bamet.lvl1$SR01Dn_Avg[sw_weird])/length(db.bamet.lvl1$SR01Dn_Avg[!is.na(db.bamet.lvl1$SR01Dn_Avg)])*100,nsmall=2 ),"%     ",          # flag=12 records [%] no NA
#                format(length(db.bamet.lvl1$IR01UpCo_Avg[lw_bad_diff])/length(db.bamet.lvl1$IR01UpCo_Avg[!is.na(db.bamet.lvl1$IR01UpCo_Avg)])*100,nsmall=2 ),"%    ",  # flag=12 records [%] no NA
#                format(length(db.bamet.lvl1$IR01DnCo_Avg[lw_bad_diff])/length(db.bamet.lvl1$IR01DnCo_Avg[!is.na(db.bamet.lvl1$IR01DnCo_Avg)])*100,nsmall=2 ),"%   (without NA) \n flag=14       ",  # flag=12 records [%] no NA
#                
#                format(length(db.bamet.lvl1$SR01Up_Avg[sw_bad_in])/length(db.bamet.lvl1$SR01Up_Avg[!is.na(db.bamet.lvl1$SR01Up_Avg)])*100,nsmall=2 ),"%    ",           # flag=14 records [%] no NA
#                format(length(db.bamet.lvl1$SR01Dn_Avg[sw_bad_out])/length(db.bamet.lvl1$SR01Dn_Avg[!is.na(db.bamet.lvl1$SR01Dn_Avg)])*100,nsmall=2 ),"%     ",         # flag=14 records [%] no NA
#                format(length(db.bamet.lvl1$IR01UpCo_Avg[lw_bad_in])/length(db.bamet.lvl1$IR01UpCo_Avg[!is.na(db.bamet.lvl1$IR01UpCo_Avg)])*100,nsmall=2 ),"%    ",     # flag=14 records [%] no NA
#                format(length(db.bamet.lvl1$IR01DnCo_Avg[lw_bad_out])/length(db.bamet.lvl1$IR01DnCo_Avg[!is.na(db.bamet.lvl1$IR01DnCo_Avg)])*100,nsmall=2 ),"%   (without NA) \n flag=15       ",  # flag=14 records [%] no NA
# 
#                format((length(snow)+0.000000000000001)/length(db.bamet$UTC)*100,nsmall=2 ),"%   snow covered sensors (without NA)\n"))                          # flag=14 records [%]   
# 
# 
#    
#    
# plotting and saving as png
#     png(paste(plot.path,jahr,"/Bayelva_rad_",jahr,".png",sep=""),width=1400,height=1000)
#     par(mfrow=c(2,1),mar=c(6.5, 4.5, 4, 2.1),omi=c(0.5,0.5,0.5,0.2))                # global plotting settings
#     
#     #plot 1
#     plot(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$SR01Up_Avg,    pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#          xlim=xxlim, ylim=ylim_sw, xlab="Date", ylab = "[W / m2]", col = 155, main="shortwave",panel.first=grid(),cex.main=2)
#     points(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$SR01Dn_Avg,  pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#            xlab="Date", ylab = "[W / m2]", col = "mediumpurple3")
#     plot_maintenance(jahr)
#     par(xpd=TRUE)
#     legend("bottomright", inset=c(0,-0.3),pch=c(19, 19),c("SR01Up_Avg (In)","SR01Dn_Avg (Out)"),col=c(155,"mediumpurple3"), bty="n", cex=1.5)
#     par(xpd=FALSE)
#     
#     # plot 2
#     plot(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$IR01UpCo_Avg,  pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#          xlim=xxlim, ylim=ylim_lw, xlab="Date", ylab = "[W / m2]", col = 155, main="longwave",panel.first=grid(),cex.main=2)
#     points(strptime(db.bamet$UTC,format="%Y-%m-%d %H:%M"),db.bamet$IR01DnCo_Avg,pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#            xlab="Date", ylab = "[W / m2]", col = "mediumpurple3")     
#     plot_maintenance(jahr)
#     par(xpd=TRUE)
#     legend("bottomright", inset=c(0,-0.3),pch=c(19, 19),c("IR01UpCo_Avg (In)","IR01DnCo_Avg (Out)"),col=c(155,"mediumpurple3"), bty="n", cex=1.5)
#     par(xpd=FALSE)
#     title( paste( "Bayelva Data for ", jahr, sep=""), line = 0, outer = TRUE, cex.main=3)   
#     dev.off()
#    
#     


#    ## LEVEL 0 plots each month
#    #  =============================================================
#    for(m in months){ 
# 
#        # extract only monthly data
#        tmp<-(m==substr(db.bamet[,1],6,7)) 
#        mm_data <- which(tmp==TRUE)
#        
#        # check if no radiation data at all exist for this month
#        if(    all(is.na(db.bamet$SR01Up_Avg[mm_data]))==TRUE   && all(is.na(db.bamet$SR01Dn_Avg[mm_data]))==TRUE 
#            && all(is.na(db.bamet$IR01UpCo_Avg[mm_data]))==TRUE && all(is.na(db.bamet$IR01DnCo_Avg[mm_data]))==TRUE ) {  
#          #cat("\nNo Data for", m, jahr)
#          next
#        }
#        
#        #cat("\nPlotting ", m, jahr)
#        ylim_sw <- plot_bounderies(db.bamet$SR01Up_Avg[mm_data],db.bamet$SR01Dn_Avg[mm_data])      # get plotting bounderies shortwave
#        ylim_lw <- plot_bounderies(db.bamet$IR01UpCo_Avg[mm_data],db.bamet$IR01DnCo_Avg[mm_data])  # get plotting bounderies longwave
#        
#        # plotting and saving as png file
#        png(paste(lvl0.path,"plots/",jahr,"/Bayelva_rad_",m,jahr,".png",sep=""),width=1400,height=1000)
#        par(mfrow=c(2,1),mar=c(6.5, 4.5, 4, 2.1),omi=c(0.5,0.5,0.5,0.2))                         # global plotting settings
#        
#        plot(strptime(db.bamet$UTC[mm_data],format="%Y-%m-%d %H:%M"),db.bamet$SR01Up_Avg[mm_data],    pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#             ylim=ylim_sw, xlab="Date", ylab = "[W / m2]", col = 155,panel.first=grid(), main="shortwave",cex.main=2)
#        points(strptime(db.bamet$UTC[mm_data],format="%Y-%m-%d %H:%M"),db.bamet$SR01Dn_Avg[mm_data],  pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#               xlab="Date", ylab = "[W / m2]", col = "mediumpurple3")
#        plot_maintenance(jahr)
#        par(xpd=TRUE)
#        legend("bottomright", inset=c(0,-0.3),pch=c(19, 19),c("SR01Up_Avg (In)","SR01Dn_Avg (Out)"),col=c(155,"mediumpurple3"), bty="n", cex=1.5)
#        par(xpd=FALSE)
#        
#        plot(strptime(db.bamet$UTC[mm_data],format="%Y-%m-%d %H:%M"),db.bamet$IR01UpCo_Avg[mm_data],  pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#             ylim=ylim_lw, xlab="Date", ylab = "[W / m2]", col = 155,panel.first=grid(), main="longwave",cex.main=2)
#        points(strptime(db.bamet$UTC[mm_data],format="%Y-%m-%d %H:%M"),db.bamet$IR01DnCo_Avg[mm_data],pch = 20, cex.lab = 1.7, cex.axis=1.7, 
#               xlab="Date", ylab = "[W / m2]", col = "mediumpurple3")     
#        plot_maintenance(jahr)
#        par(xpd=TRUE)
#        legend("bottomright", inset=c(0,-0.3),pch=c(19, 19),c("IR01UpCo_Avg (In)","IR01DnCo_Avg (Out)"),col=c(155,"mediumpurple3"), bty="n", cex=1.5)
#        par(xpd=FALSE)
#        title( paste( "Bayelva Data for ", m, "-", jahr, sep=""), line = 0, outer = TRUE, cex.main=3)   
#        dev.off()
#        
#    } # end loop over months   



library(SixSigma)

data(ss.data.wbx)
data(ss.data.wby)
plot(ss.data.wbx, ss.data.wby[, "P1"], type = "l") # "l" for line 

# Profile P1 and its smoothed version 
P1.smooth <- smoothProfiles(            
  profiles = ss.data.wby[, "P1"],
  x = ss.data.wbx)                 # smoothProfiles 使用的方法為ＳＶＭ, 產出為Matrix

plotProfiles(profiles = cbind(P1.smooth,
                              ss.data.wby[, "P1"]),
             x = ss.data.wbx)

# 所有包含在 ss.data.wby裡面的變數
plotProfiles(profiles = ss.data.wby, 
             x = ss.data.wbx)

wby.smooth <- smoothProfiles(profiles = ss.data.wby,
                             x = ss.data.wbx)
plotProfiles(profiles = wby.smooth,
             x = ss.data.wbx)


###### Phase I #####
wby.phase1 <- ss.data.wby[, 1:35]
wb.limits <- climProfiles(profiles = wby.phase1,
                          x = ss.data.wbx,
                          smoothprof = T, 
                          smoothlim = T) # T 代表上下界有smooth過
plotProfiles(profiles = wby.phase1,
             x = ss.data.wbx,
             cLimits = wb.limits) # 需為Matrix 型態且要包含prototype, confidencebands

##### Return the list of out of control profiles #####
wb.out.phase1 <- outProfiles(profiles = wby.phase1,
                             x = ss.data.wbx,
                             cLimits = wb.limits) 
wb.out.phase1 # $pOut default = 0.5  

wb.out35 <- outProfiles(profiles = wby.phase1,
                             x = ss.data.wbx,
                             cLimits = wb.limits,
                            tol = 0.8)
wb.out35

##### Without the P28 #####
wb.limits <- climProfiles(profiles = wby.phase1[, -28],
                          x = ss.data.wbx,
                          smoothprof = T,
                          smoothlim = T)

plotProfiles(profiles = wby.phase1[, -28],
             x = ss.data.wbx,
             cLimits = wb.limits)

wb.out.phase1 <- outProfiles(profiles = wby.phase1[, -28],
                             x = ss.data.wbx,
                             cLimits = wb.limits,
                             tol = 0.8)
wb.out.phase1 # 至此已確定了baseline profiles與 confidence bands

##### Phase II #####
wby.phase2 <- ss.data.wby[, 36:50]

wb.out.phase2 <- outProfiles(profiles = wby.phase2,
                             x = ss.data.wbx,
                             cLimits = wb.limits,
                             tol = 0.8)
wb.out.phase2

##### plot the Phase II profiles #####
plotProfiles(wby.phase2,
             x = ss.data.wbx,
             cLimits = wb.limits,
             outControl = wb.out.phase2$idOut)

# only containing the out-of-control profiles 
plotProfiles(wby.phase2,
             x = ss.data.wbx,
             cLimits = wb.limits,
             outControl = wb.out.phase2$idOut,
             onlyout = T)

##### Profiles control charts #####
plotControlProfiles(wb.out.phase2$pOut, tol = 0.8)

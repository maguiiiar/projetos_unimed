#BASES GERAIS - ORNELAS

basegeral201401 <- fread("BaseCusto201401.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201402 <- fread("BaseCusto201402.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201403 <- fread("BaseCusto201403.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201404 <- fread("BaseCusto201404.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201405 <- fread("BaseCusto201405.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201406 <- fread("BaseCusto201406.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201407 <- fread("BaseCusto201407.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201408 <- fread("BaseCusto201408.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201409 <- fread("BaseCusto201409.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201410 <- fread("BaseCusto201410.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201411 <- fread("BaseCusto201411.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201412 <- fread("BaseCusto201412.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201501 <- fread("BaseCusto201501.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201502 <- fread("BaseCusto201502.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201503 <- fread("BaseCusto201503.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201504 <- fread("BaseCusto201504.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201505 <- fread("BaseCusto201505.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201506 <- fread("BaseCusto201506.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201507 <- fread("BaseCusto201507.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201508 <- fread("BaseCusto201508.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201509 <- fread("BaseCusto201509.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201510 <- fread("BaseCusto201510.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201511 <- fread("BaseCusto201511.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201512 <- fread("BaseCusto201512.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201601 <- fread("BaseCusto201601.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201602 <- fread("BaseCusto201602.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201603 <- fread("BaseCusto201603.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201604 <- fread("BaseCusto201604.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201605 <- fread("BaseCusto201605.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201606 <- fread("BaseCusto201606.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201607 <- fread("BaseCusto201607.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201608 <- fread("BaseCusto201608.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201609 <- fread("BaseCusto201609.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201610 <- fread("BaseCusto201610.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201611 <- fread("BaseCusto201611.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201612 <- fread("BaseCusto201612.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201701 <- fread("BaseCusto201701.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201702 <- fread("BaseCusto201702.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201703 <- fread("BaseCusto201703.txt", h=T, sep="|",fill=T, na.string="NA")
basegeral201704 <- fread("BaseCusto201704.txt", h=T, sep="|",fill=T, na.string="NA")

basegeral <- bind_rows(basegeral201401,basegeral201402,
                       basegeral201403,basegeral201404,
                       basegeral201405,basegeral201406,
                       basegeral201407,basegeral201408,
                       basegeral201409,basegeral201410,
                       basegeral201411,basegeral201412,
                       basegeral201501,basegeral201502,
                       basegeral201503,basegeral201504,
                       basegeral201505,basegeral201506,
                       basegeral201507,basegeral201508,
                       basegeral201509,basegeral201510,
                       basegeral201511,basegeral201512,
                       basegeral201601,basegeral201602,
                       basegeral201603,basegeral201604,
                       basegeral201605,basegeral201606,
                       basegeral201607,basegeral201608,
                       basegeral201609,basegeral201610,
                       basegeral201611,basegeral201612,
                       basegeral201701,basegeral201702,
                       basegeral201703,basegeral201704)
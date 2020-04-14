
#This requires java 8, which was installed but not set as default. Rather than downgrade, there's a trick here to pick the automatic one
#sudo update-alternatives --config java
#https://askubuntu.com/questions/994515/how-to-downgrade-java-jre-the-ubuntu-way

#devtools::install_github("rstudio/sparklyr")

library(sparklyr)
#spark_install()

#options(sparklyr.java9 = TRUE)
restartspark <- function(mem="180G"){
  library(tidyverse)
  library(sparklyr)
  library(dplyr)
  library(replyr)
  library(tictoc)
  library(arrow)
  try({spark_disconnect(sc)})
  
  #https://spark.rstudio.com/deployment/
  #https://stackoverflow.com/questions/41645679/spark-driver-memory-and-executor-memory
  conf <- spark_config()
  #conf$spark.executor <- 1 #one executor per computer
  #conf$spark.executor.cores <- 24 #number of cores per executor
  #conf$"sparklyr.shell.executor-memory"="100G" #supposedly is ignored when running local
  conf$spark.memory.fraction <- 0.6
  conf$spark.executor.heartbeatInterval <-"6000000s"# "10000000s"
  conf$spark.network.timeout <- "6000001s"
  conf$spark.local.dir <- "/media/skynet2/905884f0-7546-4273-9061-12a790830beb/spark_temp/"
  conf$spark.worker.cleanup.enabled <- "true"
  #conf$spark.worker.cleanup.interval <- 60 #one minute
  
  #config$sparklyr.cores.local= 46
  #config$"sparklyr.shell.executor-cores"=4
  #config$"sparklyr.shell.driver-cores"=4
  
  conf$"sparklyr.shell.driver-memory"= mem
  conf$'spark.driver.maxResultSize' <- 0 #0 is ulimmited
  sc <<- spark_connect(master = "local", config = conf) #, version = "2.1.0" for graphframes
}

#restartspark()
#library(sparklyr)
#try({spark_disconnect(sc)})
#conf <- spark_config()
#conf$spark.executor.memory <- "120GB" #"2GB"
#conf$spark.memory.fraction <- 0.9
#conf$spark.env.SPARK_LOCAL_IP.local = "192.168.10.112" #one of these, not sure which is necessary
#conf$spark.env.SPARK_LOCAL_IP = "192.168.10.112"
#conf$spark.env.SPARK_MASTER_HOST = "192.168.10.112"
#conf$SPARK_LOCAL_IP = "192.168.10.112"
#conf$SPARK_MASTER_HOST = "192.168.10.112"
#conf$"sparklyr.shell.executor-cores"=2
#conf$"sparklyr.shell.driver-cores"=4
##conf$"sparklyr.shell.executor-memory"="120G"
#conf$"sparklyr.shell.driver-memory"="120G"
#conf$'spark.driver.maxResultSize' <- 0 #0 is ulimmited

#spark_home_set()
#sc <- spark_connect(master="spark://192.168.10.112:7077",
#              version = "2.2.0",
#              config = conf,
#              spark_home = "/home/skynet2/spark/spark-2.2.0-bin-hadoop2.7/")


#spark_installed_versions()
#spark_uninstall(version="2.4.3", hadoop_version="2.7")


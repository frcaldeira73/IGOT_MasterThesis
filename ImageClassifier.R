##--------------------------------------------------------------------
##
## Script name: ImageClassifier
##
## keywords: Google Earth Engine, Landsat, Supervised imagem classification
##
## Purpose of script: Performs classification on a LandSat imagem using points from LUCAS
## project
##
## Developed by Francisco Caldeira
##
## Version:1.0
## Date created: 09-03-2021
##
## Copyright:Modelling wheat productivity combining wheather data and Earth observation data © 2021 by Francisco Caldeira is licensed under CC BY 4.0
##
## Returns an image with the result of classification and 
## This is a function that has several parameters
## List of parameters
##
## DataSource -> It's the dataset or image where the classification it's going
##            performed this case Landsat but could be Copernicus
##
## Bands <- It's a vector with the list of bands to be used for the prediction or classification
##
## scale <- The scale or resoltion of the image
##
## BeguinDate -> The start date of the image has dd-mm-yyy
## EndDate -> The enddate of the image has dd-mm-yyy
##
## TrainingdDataFeatureClass -< Points used for training the data, the must be already on Google Earth Engine
##
## TrainingField <- The field in the TrainingdDataFeatureClass with integer value for training the model
##
## Box <- Geometry bounding box to clip the image
##
## ExporttoGCS <- Boolean argument to export the data for GCS must be already login and have an account
##
## GCS_Bucket <- Where in the GCS the imagem will be stored, must already exists withing GCS
##
## FileNamePref<- Th prefix of the imagem in the bucket
##
#--------------------------------------------------------------------

shell("cls")

library(rgee)
library(googleCloudStorageR)

ee_check_credentials()

ee_Initialize("valid.mail@gmail.com", drive = TRUE, gcs = TRUE)



#the function that does the magic
ImageClassifier <- function(DataSource, bands, imageScale, 
                            StartDate, EndDate, 
                            inferenceStartDate, inferenceEndDate,
                            TrainingdDataFeatureClass, TestingdDataFeatureClass,
                            TrainingField, Algorithm,
                            ConfusionMatrix, ErrorMatrix,
                            box,
                            ExporttoGCS, GCS_Bucket,
                            FileNamePref   ){

  
  # Input imagery is a cloud-free Landsat 8 composite.
  l8 <- ee$ImageCollection(DataSource)
   
  
  image  = ee$ImageCollection(DataSource)$filterDate(StartDate, EndDate)$median()
  image2  =  ee$ImageCollection(DataSource)$filterDate(inferenceStartDate , inferenceEndDate )$median()  
  
  
  TrainingPoints <- ee$FeatureCollection(TrainingdDataFeatureClass)
  TestingPoints <- ee$FeatureCollection(TestingdDataFeatureClass)
  
  # This property of the table stores the land cover labels.
  label <- TrainingField
  
  print( "total training points")
  ee_print( TrainingPoints)
  
  print( "total Testing points")
  ee_print( TestingPoints)
  
  
  # Overlay the points on the imagery to get training.
  training <- image$select(bands)$sampleRegions(
    collection = TrainingPoints,
    properties = list(label), #TrainingField,
    scale = imageScale
  )
  
  #*#Train the classifier Svm
  if (Algorithm == "SVM")
  {
    
    params <- ee$Classifier$libsvm(  kernelType= 'RBF',
                                     gamma= 0.5,cost= 10)
    
    classifier = params$train(training, label, bands)
    print ("The Algorithm used is SVM Support Vector Machines")
  }

  #Train the classifier Smile Cart
  if (Algorithm == "SmileCart")
  {
    classifier <- ee$Classifier$smileCart()$train(training, label, bands)
    print ("The Algorithm used is Smile Cart")
  }

  #Train the classifier Random forest
  if (Algorithm == "RandomForest")
  {
    
    
    print ("The Algorithm used is Random Forest")
    classifier <- ee$Classifier$smileRandomForest(50)$train(training, label, bands)
  }

  #Train the classifier smileGradientTreeBoost
  if (Algorithm == "smileGradientTreeBoost")
  {
    classifier <- ee$Classifier$smileGradientTreeBoost(20)$train(training, label, bands )
    print ("The Algorithm used is smileGradientTreeBoost")
  }  
  
  
  # Classify the input imge
  result <- image2$select(bands)$classify(classifier)

  
  TestingRes <- result$sampleRegions(
    collection = TestingPoints,
    properties =   list(label),
    scale = imageScale
  )
  
  #Accuracy assessment
  Testing_ErroMatrix <- TestingRes$errorMatrix("landcover", "classification")
  Testing_Accuracy <- Testing_ErroMatrix$accuracy()
  

  write.csv(ee$Number$getInfo(Testing_ErroMatrix),ErrorMatrix, row.names = TRUE)
  ModelKappa <- Testing_ErroMatrix$kappa()
  print (paste("Kappa Value of the model Validating data is:",ee$Number$getInfo(ModelKappa), sep = " ",collapse = NULL))
  
  
  print ( paste("Validating data accuracy is :",Testing_Accuracy$getInfo(), sep = " ",collapse = NULL))
  print ( paste("Validating data accuracy is :",Testing_ErroMatrix$getInfo(), sep = " ",collapse = NULL))
  
  #Get the consummer accuracy
  ModelConsumerAccuracy <- Testing_ErroMatrix$consumersAccuracy()
  print (paste("The consummer (Validating data) accuracy is: ",ee$Number$getInfo(ModelConsumerAccuracy), sep = " ",collapse = NULL))
  
  #Get the produccer accuracy
  ModelProduccerAccuracy <- Testing_ErroMatrix$producersAccuracy()
  print (paste("The produccer (Validating data) accuracy is: ",ee$Number$getInfo(ModelProduccerAccuracy), sep = " ",collapse = NULL))
  
  #Create the confusion matrix
  cfm <- classifier$confusionMatrix()
  
  #Save the confufion matrix
  write.csv(ee$Number$getInfo(cfm),ConfusionMatrix, row.names = TRUE)
  
  
  if (ExporttoGCS == TRUE)
  {
  
    CippedImage <-result$clip(box) 
    
    task_img <- ee_image_to_gcs(
      image = CippedImage, 
      bucket = GCS_Bucket,
      fileFormat = "GEO_TIFF",
      region = box,
      scale = scale,
      fileNamePrefix =   FileNamePref  
    )
    
    task_img$start()
    ee_monitoring(task_img)
    
  }
  
  print ("Exit funcion")
  
}

#Parameters the run the prediction/classification
#Parameters to define image context
MyDataSource <- "LANDSAT/LC08/C01/T1_SR"  #"LANDSAT/LT05/C01/T1_SR"   #"LANDSAT/LE07/C01/T1_SR"   
MyStartDate <- "2018-03-01" 
MyEndDate <-   "2018-05-31" 
MyinferenceStartDate <- "2018-03-01" 
MyinferenceEndDate  <- "2018-05-31" 
MyBands <- c( "B1","B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11")    #c("B1", 'B2', 'B3', 'B4', 'B5', "B6",  'B7' )  
Myscale <- 30


#Parameter to define the chosen algorithm
#The valid options are:
#SmileCart
#SVM
#RandomForest
#smileGradientTreeBoost
#NaiveBayes is an option not implemented
myAlgorithm <- "RandomForest" 

#PArameters to define Training and testing data
MyTrainingdDataFeatureClass <- "users/ine/LUCAS2018T5"
MyTestingdDataFeatureClass <- "users/ine/LUCAS2018V5"   


MyTrainingField <- "landcover"

#Parameters to define the location of the confusion matrixes
MyConfusionMatrix <- "C:\\lab\\mestrado\\TESE\\DADOS\\ImageClassification\\ConfusionMatrix2018.csv"
MyErrorMatrix <- "C:\\lab\\mestrado\\TESE\\DADOS\\ImageClassification\\ErrorConfusionMatrix2018.csv"

#Parameters to define the export context into GCS
MyExporttoGCS <- FALSE
MyGCS_Bucket <- "rgee_model_wheat"
MyFileNamePref <- "IMG_Class_Alentejo_rf2009"

#Bounding box for Alentejo NUTS-II
Mybox <- ee$Geometry$Rectangle(coords = c(-9.2, 37.2,-6.8,39.7),proj = "EPSG:4326", geodesic = FALSE)


ImageClassifier(DataSource = MyDataSource, bands = MyBands, imageScale= Myscale,
                StartDate = MyStartDate, EndDate = MyEndDate,
                inferenceStartDate = MyinferenceStartDate, inferenceEndDate = MyinferenceEndDate,
                TrainingdDataFeatureClass = MyTrainingdDataFeatureClass,
                TestingdDataFeatureClass = MyTestingdDataFeatureClass,
                TrainingField = MyTrainingField,
                Algorithm = myAlgorithm,
                ConfusionMatrix = MyConfusionMatrix,
                ErrorMatrix = MyErrorMatrix,
                box =  Mybox,
                ExporttoGCS = MyExporttoGCS, GCS_Bucket = MyGCS_Bucket,
                FileNamePref = MyFileNamePref  )


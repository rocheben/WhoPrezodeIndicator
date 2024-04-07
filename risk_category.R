risk_category <- function(wildlifePressure,wildlifeContact, livestockPressure,livestockContact,livestockWildlife){

  if(is.na(wildlifePressure) || is.na(wildlifeContact)){
      wildlifeRisk=NA;
    }
    else{
      if(wildlifePressure==0 || wildlifeContact==0){
        wildlifeRisk=0;
      }
      if(wildlifePressure==1 && wildlifeContact==1){
        wildlifeRisk=1;
      }
      if( (wildlifePressure==1 && wildlifeContact==2) || (wildlifePressure==2 && wildlifeContact==1) ){
        wildlifeRisk=2;
      }
      if( (wildlifePressure==1 && wildlifeContact==3) || (wildlifePressure==3 && wildlifeContact==1) || (wildlifePressure==2 && wildlifeContact==2) ){
        wildlifeRisk=3;
      }
      if( (wildlifePressure==2 && wildlifeContact==3) || (wildlifePressure==3 && wildlifeContact==2) ){
        wildlifeRisk=4;
      }
      if(wildlifePressure==3 && wildlifeContact==3){
        wildlifeRisk=5;
      }
    }
    if(is.na(livestockPressure) || is.na(livestockContact)){
      livestockRisk=NA;
    }
    else{
      if(livestockPressure==0 || livestockContact==0){
        livestockRisk=0;
      }
      if(livestockPressure==1 && livestockContact==1){
        livestockRisk=1;
      }
      if( (livestockPressure==1 && livestockContact==2) || (livestockPressure==2 && livestockContact==1) ){
        livestockRisk=2;
      }
      if( (livestockPressure==1 && livestockContact==3) || (livestockPressure==3 && livestockContact==1) || (livestockPressure==2 && livestockContact==2) ){
        livestockRisk=3;
      }
      if( (livestockPressure==2 && livestockContact==3) || (livestockPressure==3 && livestockContact==2) ){
        livestockRisk=4;
      }
     if(livestockPressure==3 && livestockContact==3){
        livestockRisk=5;
      }
    }

    if(is.na(livestockRisk) || is.na(wildlifeRisk)){
      compositeRisk=NA;
    }
    else{
      if(livestockRisk==0){
        compositeRisk=wildlifeRisk;
      }
      if(wildlifeRisk==0){
        compositeRisk=livestockRisk;
      }
      if(livestockRisk==1 && wildlifeRisk==1){
        compositeRisk=1;
      }
      if( (livestockRisk==1 && wildlifeRisk==2 ) || (livestockRisk==2 && wildlifeRisk==1 ) ) {
        compositeRisk=2;
      }
      if( (livestockRisk==3 && wildlifeRisk==1 ) || (livestockRisk==1 && wildlifeRisk==3 ) || (livestockRisk==2 && wildlifeRisk==2 ) ) {
        compositeRisk=3;
      }
      if( (livestockRisk==4 && wildlifeRisk==1 ) || (livestockRisk==1 && wildlifeRisk==4 ) || (livestockRisk==2 && wildlifeRisk==3 ) || (livestockRisk==3 && wildlifeRisk==2 )) {
        compositeRisk=4;
      }
      if( (livestockRisk==5 && wildlifeRisk==1 ) || (livestockRisk==1 && wildlifeRisk==5 ) || (livestockRisk==2 && wildlifeRisk==4 ) || (livestockRisk==4 && wildlifeRisk==2 )  || (livestockRisk==3 && wildlifeRisk==3 ) ) {
        compositeRisk=5;
      }
      if( (livestockRisk==5 && wildlifeRisk==2 ) || (livestockRisk==2 && wildlifeRisk==5 ) || (livestockRisk==3 && wildlifeRisk==4 ) || (livestockRisk==4 && wildlifeRisk==3 ) ) {
        compositeRisk=6;
      }
      if( (livestockRisk==5 && wildlifeRisk==3 ) || (livestockRisk==3 && wildlifeRisk==5 ) || (livestockRisk==4 && wildlifeRisk==4 ) ) {
        compositeRisk=7;
      }
      if( (livestockRisk==5 && wildlifeRisk==4 ) || (livestockRisk==4 && wildlifeRisk==5 ) ) {
        compositeRisk=8;
      }
      if( (livestockRisk==5 && wildlifeRisk==5) ){
        compositeRisk=9;
      }
    }
    return(c(wildlifeRisk,livestockRisk,compositeRisk))
}

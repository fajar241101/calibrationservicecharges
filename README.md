## Calibration Service Charges Payment Portals

This Application is Service Charges Payment Portal for Instrument Calibration Activity

The idea of the program development is current conditions Instrument Calibration Service Provider worked based on centralized database which are mostly working on manual validation.

This program worked through validation of calibration report submit by service provider to check and validate the calibration on spesification target with calibration validation value.

If the calibration result achieved, the service provider could automatically withdrawn the service fee.

To prevent unauthorized withdrawl, the application has been protected by authorization pin and public hash key address the service provider/ beneficiary.

## Application Execution
This Application run on Plutus Background Environment and to perform simulation open the Plutus Test Background Environment through website https://playground.plutus.iohkdev.io/.

Copy and paste the plutus program and executed the files.

Perform Simulation using Positive and Negative Test


## Application Positive Test
To simulate the Reward Application can go through positive test which are follow condition :

a. Validation Zero Point Calibration Value shall be the same with Target Baseline Zero Calibration On Specification, and

b. Validation Span Point Calibration Value shall be the same with Target Baseline Span Calibration On Specification, and

c. Validation Percentage Accuracy Calibration Value shall be the same or below Target Baseline Percentage Accuracy Calibration On Specification, and

d. Validation Percentage Repeatability Calibration Value shall be the same or below Target Baseline Percentage Repeatability Calibration On Specification, and

e. Address Service Provider Beneficiary shall be the same with target Service Provider Beneficiary, and

f. Pin Reward Service Charges Withdrawl Authorization to release the Service Charges Rewards shall be the same


## Application Negative Test
To simulate the Reward Application can go through negative test which are follow condition :

a. Validation Zero Point Calibration Value not the same with Target Baseline Zero Calibration On Specification, and

b. Validation Span Point Calibration Value not the same with Target Baseline Span Calibration On Specification, and

c. Validation Percentage Accuracy Calibration Value not the same or above Target Baseline Percentage Accuracy Calibration On Specification, and

d. Validation Percentage Repeatability Calibration Value not the same or above Target Baseline Percentage Repeatability Calibration On Specification, and

e. Address Service Provider Beneficiary not the same with target Service Provider Beneficiary, and

f. Pin Reward Service Charges Withdrawl Authorization to release the Service Charges Rewards not the same


#### Figure 1 Positive Simulation Test - Calibration Service Charges Payment

![CalPositiveTest1](https://user-images.githubusercontent.com/115913889/209138758-c9c0a7c1-fc17-42de-b129-893f86def444.png)

#### Figure 2 Positive Simulation Test Result - Calibration Service Charges Payment

![CalPositiveTest2](https://user-images.githubusercontent.com/115913889/209138795-6f3c5d85-7954-46d2-a279-3057d963f342.png)

#### Figure 3 Positive Simulation Test - Calibration Service Charges Payment

![CalNegativeTest1](https://user-images.githubusercontent.com/115913889/209138842-633412bf-2b69-4a60-88ea-4a1a54a93d08.png)

#### Figure 4 Positive Simulation Test Result - Calibration Service Charges Payment

![CalNegativeTest2](https://user-images.githubusercontent.com/115913889/209138872-a592e10c-4b23-458e-a147-ad8e44a2c603.png)

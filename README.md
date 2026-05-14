# 🥈 Predictive Maintenance: CRPI Lab Data Challenge (2nd Place Solution)

> **Predictive maintenance pipeline to anticipate industrial machinery failures from sequential sensor data. Achieved 2nd place in the 2025-26 Lab Data Challenge organized by UNIMIB in collaboration with CRPI Digital.**

## 📌 Project Overview
In modern Industry 4.0 environments, preventing machine downtime is critical, but predicting rare mechanical failures is highly complex. This project develops a robust, multi-model Machine Learning pipeline to predict specific types of machinery malfunctions based on a simulated Digital Twin dataset. 

Our approach prioritizes **statistical rigor, physical interpretability, and business-value optimization** over the blind application of black-box algorithms.

### The Team (ThreeShots)
* **Luca Iaria** * **Giacomo Fullin**
* **Federico Mariani**

---

## 📊 The Data & The Challenge
The dataset consists of **8,000 sequential observations** representing single production cycles. The features include operational parameters (Rotations per hour, Torque, Processing time, Product tier) and observed parameters (External temperature, Process temperature).

**The Core Challenge: Extreme Class Imbalance.** Failures in the dataset are exceptionally rare, making standard predictive modeling ineffective. 

| Target State | Sane (0) | Failure (1) | Frequency |
| :--- | :--- | :--- | :--- |
| **Global Failure (mal0)** | 7707 | 293 | 3.66% |
| **Malfunction 1 (Prolonged Stress)** | 7964 | 36 | **0.45%** |
| **Malfunction 2 (Overheating)** | 7885 | 115 | 1.43% |
| **Malfunction 3 (Power Overload)** | 7915 | 85 | 1.06% |
| **Malfunction 4 (Mechanical Stress)** | 7923 | 77 | 0.96% |
| **Malfunction 5 (Stochastic)** | 7981 | 19 | 0.23% |

---

## 🔬 Methodology

### 1. Physics-Based Feature Engineering
Raw sensor data alone did not encode the "wear and tear" state of the machinery. We engineered domain-specific features to capture mechanical degradation:
* **Wear Stress:** `Processing Time * Torque`
* **Thermal Delta:** `Process Temp - External Temp` (captures dissipation inefficiency)
* **Moving Variance of Torque:** 5-step rolling window to detect dynamic instability and vibrations.

### 2. Preventing Temporal Data Leakage
Since the data is sequential, random shuffling would cause severe data leakage (using the future to predict the past). 
* **Growing Window Cross-Validation:** We implemented a custom time-series cross-validation (50% initial train, expanding by 16.7% per fold) to tune hyperparameters.
  <p align="center">
  <img width="526" height="299" alt="Screenshot 2026-05-14 alle 14 57 23" src="https://github.com/user-attachments/assets/faff12ea-98fe-4899-a2dc-36bb09f35108" />
</p>
* **Isolated Oversampling:** We applied **SMOTE** strictly *inside* the training folds. The validation sets remained untouched and naturally imbalanced to ensure unbiased performance estimates.

### 3. Model Optimization (F2-Score)
In an industrial context, missing a real failure (False Negative) is vastly more expensive than triggering a false alarm (False Positive). Therefore, the Random Search for hyperparameter tuning was specifically designed to maximize the **F2-Score**, effectively weighting Recall twice as much as Precision.

---

## 🚀 Results & Interpretability

We trained independent **Random Forest** models for each malfunction type, as they demonstrated the best resilience against overfitting and the ability to capture non-linear sensor interactions. Tested on a final hold-out set of 2,000 sequential observations (containing 41 global failures), the models yielded the following business insights:

* **Malfunctions 2, 3, & 4:** Achieved near-perfect Balanced Accuracy, successfully detecting the actual failures in the test set (0 for Mal2, 10 for Mal3, and 21 for Mal4). The Gini Variable Importance confirmed the physical nature of the faults (e.g., *Thermal Delta* driving Malfunction 2, *Power* driving Malfunction 3).

<p align="center">
  <img width="549" alt="Hyperparameters Table" src="https://github.com/user-attachments/assets/a568202a-7a80-4758-89ba-5061374df00b" />
</p>
<p align="center">
  <img width="838" alt="Variable Importance 2,3,4" src="https://github.com/user-attachments/assets/836ea2b6-2d40-40b1-a9a4-1822adb16cac" />
</p>

* **Malfunction 1 (The Cost-Matrix Dilemma):** This was the most complex target, with only 10 actual failures in the final test set. Our conservative model triggered 121 alarms. While the algorithm successfully identified the physical precursor (*Wear Stress*), this highlights a critical business trade-off: **maintenance costs vs. risk**. Dispatching a technician over 100 times for false positives carries a heavy operational cost. We deliberately tuned the threshold to minimize missed catastrophic failures, but the final implementation requires management to weigh the financial cost of continuous inspections against the cost of machine downtime.<img width="821" height="262" alt="Screenshot 2026-05-14 alle 14 58 05" src="https://github.com/user-attachments/assets/106d0239-1862-4879-bd1f-d83cc77d46d4" />

* **Malfunction 5 (The Stochastic Case):** Data exploration revealed a 0% physical predictability for this failure type (which had 0 occurrences in the test set). Recognizing the stochastic nature of the fault, we recommended a purely reactive maintenance strategy rather than forcing a Machine Learning model that would only generate noise and alarm fatigue.

---

## ⚠️ Data Availability
The original dataset is not included in this repository as it belongs to the CRPI Digital challenge organizers. 
The pipeline is designed to run on any dataset with the same feature structure.


## 📂 Repository Structure
* `script.R`: The complete, unified R pipeline including EDA, Feature Engineering, Growing Window CV, SMOTE modeling, and evaluation.
* `presentation.pdf`: The final slide deck presented to CRPI Digital management, focusing on business impact and physical interpretability.

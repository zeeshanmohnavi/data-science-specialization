## Data Dictionary

Refer to the [README](./README.md#token-desc) for a description of the tokens used to create the descriptive variable names.
  
| **Column Index** | **Variable Name** | **Data Type** | **Labels** | **Values** |
|------------------|-------------------|---------------|------------|------------|
| 1 | subject | numeric | Identifier for the person on whom the experiment was conducted | [1, 30] |
| 2 | activity | character | Activity name | `WALKING`, `WALKING_UPSTAIRS`, `WALKING_DOWNSTAIRS`, `SITTING`, `STANDING`, `LAYING` |
| 3 | time_body_accelerometer_mean_X_axis | numeric | Mean of the mean of the body acceleration signals captured using an accelerometer along the X-axis in time-domain |  |
| 4 | time_body_accelerometer_mean_Y_axis | numeric | Mean of the mean of the body acceleration signals captured using an accelerometer along the Y-axis in time-domain |  |
| 5 | time_body_accelerometer_mean_Z_axis | numeric | Mean of the mean of the body acceleration signals captured using an accelerometer along the Z-axis in time-domain |  |
| 6 | time_body_accelerometer_sd_X_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using an accelerometer along the X-axis in time-domain |  |
| 7 | time_body_accelerometer_sd_Y_axis | numeric | Mean of the standard deviation of the body acceleration signals  captured using an accelerometer along the Y-axis in time-domain |  |
| 8 | time_body_accelerometer_sd_Z_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using an accelerometer along the Z-axis in time-domain |  |
| 9 | time_gravity_accelerometer_mean_X_axis | numeric | Mean of the mean of the gravity acceleration signals captured using an accelerometer along the X-axis in time-domain |  |
| 10 | time_gravity_accelerometer_mean_Y_axis | numeric | Mean of the mean of the gravity acceleration signals captured using an accelerometer along the Y-axis in time-domain |  |
| 11 | time_gravity_accelerometer_mean_Z_axis | numeric |Mean of the mean of the gravity acceleration signals captured using an accelerometer along the Z-axis in time-domain  |  |
| 12 | time_gravity_accelerometer_sd_X_axis | numeric | Mean of the standard deviation of the gravity acceleration signals captured using an accelerometer along the X-axis in time-domain |  |
| 13 | time_gravity_accelerometer_sd_Y_axis | numeric | Mean of the standard deviation of the gravity acceleration signals captured using an accelerometer along the Y-axis in time-domain |  |
| 14 | time_gravity_accelerometer_sd_Z_axis | numeric | Mean of the standard deviation of the gravity acceleration signals captured using an accelerometer along the Z-axis in time-domain |  |
| 15 | time_body_accelerometer_jerk_mean_X_axis | numeric | Mean of the mean of the linear acceleration of the body along the X-axis in time-domain |  |
| 16 | time_body_accelerometer_jerk_mean_Y_axis | numeric | Mean of the mean of the linear acceleration of the body along the Y-axis in time-domain |  |
| 17 | time_body_accelerometer_jerk_mean_Z_axis | numeric | Mean of the mean of the linear acceleration of the body along the Z-axis in time-domain |  |
| 18 | time_body_accelerometer_jerk_sd_X_axis | numeric | Mean of the standard deviation of the linear acceleration of the body along the X-axis in time-domain |  |
| 19 | time_body_accelerometer_jerk_sd_Y_axis | numeric | Mean of the standard deviation of the linear acceleration of the body along the Y-axis in time-domain |  |
| 20 | time_body_accelerometer_jerk_sd_Z_axis | numeric | Mean of the standard deviation of the linear acceleration of the body along the Z-axis in time-domain |  |
| 21 | time_body_gyrometer_mean_X_axis | numeric | Mean of the mean of the body acceleration signals captured using a gyrometer along the X-axis in time-domain |  |
| 22 | time_body_gyrometer_mean_Y_axis | numeric | Mean of the mean of the body acceleration signals captured using a gyrometer along the Y-axis in time-domain |  |
| 23 | time_body_gyrometer_mean_Z_axis | numeric | Mean of the mean of the body acceleration signals captured using a gyrometer along the Z-axis in time-domain |  |
| 24 | time_body_gyrometer_sd_X_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using a gyrometer along the X-axis in time-domain |  |
| 25 | time_body_gyrometer_sd_Y_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using a gyrometer along the Y-axis in time-domain |  |
| 26 | time_body_gyrometer_sd_Z_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using a gyrometer along the Z-axis in time-domain |  |
| 27 | time_body_gyrometer_jerk_mean_X_axis | numeric | Mean of the mean of the angular velocity of the body along the X-axis in time-domain |  |
| 28 | time_body_gyrometer_jerk_mean_Y_axis | numeric | Mean of the mean of the angular velocity of the body along the Y-axis in time-domain |  |
| 29 | time_body_gyrometer_jerk_mean_Z_axis | numeric | Mean of the mean of the angular velocity of the body along the Z-axis in time-domain |  |
| 30 | time_body_gyrometer_jerk_sd_X_axis | numeric | Mean of the standard deviation of the angular velocity of the body along the X-axis in time-domain |  |
| 31 | time_body_gyrometer_jerk_sd_Y_axis | numeric | Mean of the standard deviation of the angular velocity of the body along the Y-axis in time-domain |  |
| 32 | time_body_gyrometer_jerk_sd_Z_axis | numeric | Mean of the standard deviation of the angular velocity of the body along the Z-axis in time-domain |  |
| 33 | time_body_accelerometer_magnitude_mean | numeric | Mean of the mean of the magnitude of the body acceleration signals captured using an accelerometer in time-domain |  |
| 34 | time_body_accelerometer_magnitude_sd | numeric | Mean of the standard deviation of the magnitude of th body acceleration signals captured using an accelerometer in time-domain |  |
| 35 | time_gravity_accelerometer_magnitude_mean | numeric | Mean of the mean of the magnitude of the gravity acceleration signals captured using an accelerometer in time-domain |  |
| 36 | time_gravity_accelerometer_magnitude_sd | numeric | Mean of the standard deviation of the magnitude of the gravity acceleration signals captured using an accelerometer in time-domain |  |
| 37 | time_body_accelerometer_jerk_magnitude_mean | numeric | Mean of the mean of the magnitude of the linear acceleration of the body in time-domain |  |
| 38 | time_body_accelerometer_jerk_magnitude_sd | numeric | Mean of the standard deviation of the magnitude of the linear acceleration of the body in time-domain |  |
| 39 | time_body_gyrometer_magnitude_mean | numeric | Mean of the mean of the magnitude of the body acceleration signals captured using a gyrometer in time-domain |  |
| 40 | time_body_gyrometer_magnitude_sd | numeric | Mean of the standard deviation of the body acceleration signals captured using a gyrometer in time-domain |  |
| 41 | time_body_gyrometer_jerk_magnitude_mean | numeric | Mean of the mean of the magnitude of the angular velocity of the body in time-domain |  |
| 42 | time_body_gyrometer_jerk_magnitude_sd | numeric | Mean of the standard deviation of the magnitude of the angular velocity of the body in time-domain |  |
| 43 | freq_body_accelerometer_mean_X_axis | numeric | Mean of the mean of the body acceleration signals captured using an accelerometer along the X-axis in frequency-domain |  |
| 44 | freq_body_accelerometer_mean_Y_axis | numeric | Mean of the mean of the body acceleration signals captured using an accelerometer along the Y-axis in frequency-domain |  |
| 45 | freq_body_accelerometer_mean_Z_axis | numeric | Mean of the mean of the body acceleration signals captured using an accelerometer along the Z-axis in frequency-domain |  |
| 46 | freq_body_accelerometer_sd_X_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using an accelerometer along the X-axis in frequency-domain |  |
| 47 | freq_body_accelerometer_sd_Y_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using an accelerometer along the Y-axis in frequency-domain |  |
| 48 | freq_body_accelerometer_sd_Z_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using an accelerometer along the Z-axis in frequency-domain |  |
| 49 | freq_body_accelerometer_jerk_mean_X_axis | numeric | Mean of the mean of the linear acceleration of the body along the X-axis in frequency-domain |  |
| 50 | freq_body_accelerometer_jerk_mean_Y_axis | numeric | Mean of the mean of the linear acceleration of the body along the Y-axis in frequency-domain |  |
| 51 | freq_body_accelerometer_jerk_mean_Z_axis | numeric | Mean of the mean of the linear acceleration of the body along the Z-axis in frequency-domain |  |
| 52 | freq_body_accelerometer_jerk_sd_X_axis | numeric | Mean of the standard deviation of the linear acceleration of the body along the X-axis in frequency-domain |  |
| 53 | freq_body_accelerometer_jerk_sd_Y_axis | numeric | Mean of the standard deviation of the linear acceleration of the body along the Y-axis in frequency-domain |  |
| 54 | freq_body_accelerometer_jerk_sd_Z_axis | numeric | Mean of the standard deviation of the linear acceleration of the body along the Z-axis in frequency-domain |  |
| 55 | freq_body_gyrometer_mean_X_axis | numeric | Mean of the mean of the body acceleration signals captured using a gyrometer along the X-axis in frequency-domain |  |
| 56 | freq_body_gyrometer_mean_Y_axis | numeric | Mean of the mean of the body acceleration signals captured using a gyrometer along the Y-axis in frequency-domain |  |
| 57 | freq_body_gyrometer_mean_Z_axis | numeric | Mean of the mean of the body acceleration signals captured using a gyrometer along the Z-axis in frequency-domain |  |
| 58 | freq_body_gyrometer_sd_X_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using a gyrometer along the X-axis in frequency-domain |  |
| 59 | freq_body_gyrometer_sd_Y_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using a gyrometer along the Y-axis in frequency-domain |  |
| 60 | freq_body_gyrometer_sd_Z_axis | numeric | Mean of the standard deviation of the body acceleration signals captured using a gyrometer along the Z-axis in frequency-domain |  |
| 61 | freq_body_accelerometer_magnitude_mean | numeric | Mean of the mean of the magnitude of the body acceleration signals captured using an accelerometer in frequency-domain |  |
| 62 | freq_body_accelerometer_magnitude_sd | numeric | Mean of the standard deviation of the magnitude of the body acceleration signals captured using an accelerometer in frequency-domain |  |
| 63 | freq_body_accelerometer_jerk_magnitude_mean | numeric | Mean of the mean of the magnitude of the linear acceleration of the body in frequency-domain |  |
| 64 | freq_body_accelerometer_jerk_magnitude_sd | numeric | Mean of the standard deviation of the magnitude of the linear acceleration of the body in frequency-domain |  |
| 65 | freq_body_gyrometer_magnitude_mean | numeric | Mean of the mean of the magnitude of the body acceleration signals in frequency-domain |  |
| 66 | freq_body_gyrometer_magnitude_sd | numeric | Mean of the standard deviation of the magnitude of the body acceleration signals in frequency-domain |  |

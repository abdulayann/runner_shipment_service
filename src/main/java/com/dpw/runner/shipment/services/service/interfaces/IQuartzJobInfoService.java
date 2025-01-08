package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.messaging.api.response.QuartzJobResponse;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;

import java.time.LocalDateTime;

public interface IQuartzJobInfoService {
    QuartzJobResponse createSimpleJob(QuartzJobInfo jobInfo);
    QuartzJobResponse updateSimpleJob(QuartzJobInfo jobInfo);
    QuartzJobResponse deleteJobById(Long jobId);
    LocalDateTime getQuartzJobTime(LocalDateTime eta, LocalDateTime etd, LocalDateTime ata, LocalDateTime atd);
    boolean isJobWithNamePresent(String jobName);

}

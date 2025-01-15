package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.messaging.api.response.QuartzJobResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;

import java.time.LocalDateTime;
import java.util.List;

public interface IQuartzJobInfoService {
    QuartzJobResponse createSimpleJob(QuartzJobInfo jobInfo);
    QuartzJobResponse updateSimpleJob(QuartzJobInfo jobInfo);
    QuartzJobResponse deleteJobById(Long jobId);
    LocalDateTime getQuartzJobTime(LocalDateTime eta, LocalDateTime etd, LocalDateTime ata, LocalDateTime atd);
    List<V1TenantSettingsResponse.FileTransferConfigurations> getActiveFileTransferConfigurations();
    boolean isJobWithNamePresent(String jobName);

}

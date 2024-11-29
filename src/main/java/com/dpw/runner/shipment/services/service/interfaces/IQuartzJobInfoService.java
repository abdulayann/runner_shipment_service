package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.messaging.api.response.QuartzJobResponse;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;

public interface IQuartzJobInfoService {
    QuartzJobResponse createSimpleJob(QuartzJobInfo jobInfo);
    QuartzJobResponse deleteJobById(Long jobId);

}

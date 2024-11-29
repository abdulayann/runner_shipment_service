package com.dpw.runner.shipment.services.service.impl;

import com.dpw.api.quartz.job.SimpleJob;
import com.dpw.api.quartz.service.QuartzJobService;
import com.dpw.messaging.api.response.QuartzJobResponse;
import com.dpw.runner.shipment.services.commons.constants.QuartzJobInfoConstants;
import com.dpw.runner.shipment.services.dao.impl.QuartzJobInfoDao;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.entity.enums.JobState;
import com.dpw.runner.shipment.services.service.interfaces.IQuartzJobInfoService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

@Slf4j
@Service
public class QuartzJobInfoService implements IQuartzJobInfoService {

    private final QuartzJobService quartzJobService;
    private final IQuartzJobInfoDao quartzJobInfoDao;

    @Autowired
    QuartzJobInfoService(QuartzJobService quartzJobService, QuartzJobInfoDao quartzJobInfoDao) {
        this.quartzJobService = quartzJobService;
        this.quartzJobInfoDao = quartzJobInfoDao;
    }

    public QuartzJobResponse createSimpleJob(QuartzJobInfo jobInfo) {
        jobInfo.setJobStatus(JobState.QUEUED);
        jobInfo = quartzJobInfoDao.save(jobInfo);
        LocalDateTime localDateTime = jobInfo.getStartTime();
        ZoneId zoneId = ZoneId.of("UTC");
        ZonedDateTime zonedDateTime = localDateTime.atZone(zoneId);
        Instant instant = zonedDateTime.toInstant();
        Date startDate = Date.from(instant);
        long startTimeForQuartz = System.currentTimeMillis();
        QuartzJobResponse quartzJobResponse = quartzJobService.scheduleOneTimeJob(jobInfo.getId().toString(), SimpleJob.class, startDate);
        log.info(QuartzJobInfoConstants.TIME_TAKE_TO_SCHEDULE_JOB, jobInfo.getId().toString(),
                System.currentTimeMillis()-startTimeForQuartz);
        return quartzJobResponse;
    }

    @Transactional
    public QuartzJobResponse deleteJobById(Long jobId) {
        long startTimeForQuartz = System.currentTimeMillis();
        quartzJobInfoDao.deleteById(jobId);
        QuartzJobResponse quartzJobResponse = quartzJobService.deleteJob(jobId.toString());
        log.info(QuartzJobInfoConstants.TIME_TAKE_TO_SCHEDULE_JOB, jobId,
                System.currentTimeMillis()-startTimeForQuartz);
        return quartzJobResponse;
    }


}

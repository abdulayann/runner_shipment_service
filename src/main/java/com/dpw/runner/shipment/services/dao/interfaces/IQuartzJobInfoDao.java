package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.QuartzJobInfo;

import java.util.List;
import java.util.Optional;

public interface IQuartzJobInfoDao {
    QuartzJobInfo save(QuartzJobInfo quartzJobInfo);
    void delete(QuartzJobInfo quartzJobInfo);
    Optional<QuartzJobInfo> findById(Long id);
    List<QuartzJobInfo> saveAll(List<QuartzJobInfo> quartzJobInfoList);
    void deleteById(Long id);
}

package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.repository.interfaces.IQuartzJobInfoRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Slf4j
@Repository
public class QuartzJobInfoDao implements IQuartzJobInfoDao {

    private final IQuartzJobInfoRepository quartzJobInfoRepository;

    @Autowired
    QuartzJobInfoDao(IQuartzJobInfoRepository quartzJobInfoRepository) {
        this.quartzJobInfoRepository = quartzJobInfoRepository;
    }

    @Override
    public QuartzJobInfo save(QuartzJobInfo quartzJobInfo) {
        return quartzJobInfoRepository.save(quartzJobInfo);
    }

    @Override
    public void delete(QuartzJobInfo quartzJobInfo) {
        quartzJobInfoRepository.delete(quartzJobInfo);
    }

    @Override
    public Optional<QuartzJobInfo> findById(Long id) {
        return quartzJobInfoRepository.findById(id);
    }

    @Override
    public List<QuartzJobInfo> saveAll(List<QuartzJobInfo> quartzJobInfoList) {
        return quartzJobInfoRepository.saveAll(quartzJobInfoList);
    }

    @Override
    public void deleteById(Long id) {
        quartzJobInfoRepository.deleteById(id);
    }

    @Override
    public Optional<QuartzJobInfo> findByJobFilters(Integer tenantId, Long entityId, String entityType){
        return quartzJobInfoRepository.findByJobFilters(tenantId, entityId, entityType);
    }

    @Override
    public Optional<QuartzJobInfo> findByIdQuery(Long id) {
        return quartzJobInfoRepository.findByIdQuery(id);
    }
}

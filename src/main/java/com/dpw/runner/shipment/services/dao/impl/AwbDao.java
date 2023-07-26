package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
@Slf4j
public class AwbDao implements IAwbDao {
    @Autowired
    private IAwbRepository awbShipmentInfoRepository;
    @Override
    public Awb save(Awb awbShipmentInfo) {
        return awbShipmentInfoRepository.save(awbShipmentInfo);
    }

    @Override
    public Page<Awb> findAll(Specification<Awb> spec, Pageable pageable) {
        return awbShipmentInfoRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Awb> findById(Long id) {
        return awbShipmentInfoRepository.findById(id);
    }
}

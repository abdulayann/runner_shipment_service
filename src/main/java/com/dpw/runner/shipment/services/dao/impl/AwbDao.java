package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class AwbDao implements IAwbDao {
    @Autowired
    private IAwbRepository awbRepository;
    @Override
    public Awb save(Awb awbShipmentInfo) {
        return awbRepository.save(awbShipmentInfo);
    }

    @Override
    public Page<Awb> findAll(Specification<Awb> spec, Pageable pageable) {
        return awbRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Awb> findById(Long id) {
        return awbRepository.findById(id);
    }

    @Override
    public List<Awb> findByShipmentId(Long shipmentId) {
        return awbRepository.findByShipmentId(shipmentId);
    }

    @Override
    public List<Awb> findByConsolidationId(Long consolidationId) {return awbRepository.findByConsolidationId(consolidationId);}
}

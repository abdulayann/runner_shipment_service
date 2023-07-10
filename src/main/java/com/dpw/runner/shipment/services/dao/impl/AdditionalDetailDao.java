package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import com.dpw.runner.shipment.services.repository.interfaces.IAdditionalDetailRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class AdditionalDetailDao implements IAdditionalDetailDao {
    @Autowired
    private IAdditionalDetailRepository additionalDetailRepository;

    @Override
    public AdditionalDetail save(AdditionalDetail additionalDetail) {
        return additionalDetailRepository.save(additionalDetail);
    }
}

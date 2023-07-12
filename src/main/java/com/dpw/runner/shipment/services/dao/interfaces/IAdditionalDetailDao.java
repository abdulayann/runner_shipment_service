package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IAdditionalDetailDao {
    AdditionalDetail save(AdditionalDetail additionalDetail);
    Page<AdditionalDetail> findAll(Specification<AdditionalDetail> spec, Pageable pageable);
    Optional<AdditionalDetail> findById(Long id);
    void delete(AdditionalDetail additionalDetail);
    AdditionalDetail updateEntityFromShipment(AdditionalDetail additionalDetail, Long shipmentId) throws Exception;
}

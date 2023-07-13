package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IAdditionalDetailDao {
    AdditionalDetails save(AdditionalDetails additionalDetails);

    Page<AdditionalDetails> findAll(Specification<AdditionalDetails> spec, Pageable pageable);

    Optional<AdditionalDetails> findById(Long id);

    void delete(AdditionalDetails additionalDetails);

    AdditionalDetails updateEntityFromShipment(AdditionalDetails additionalDetail, Long shipmentId) throws Exception;
}

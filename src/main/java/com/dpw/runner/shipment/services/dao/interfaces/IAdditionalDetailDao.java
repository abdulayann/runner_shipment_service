package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.List;
import java.util.Optional;

public interface IAdditionalDetailDao {
    AdditionalDetails save(AdditionalDetails additionalDetails);

    Optional<AdditionalDetails> findById(Long id);

    AdditionalDetails updateEntityFromShipment(AdditionalDetails additionalDetail) throws RunnerException;

    List<AdditionalDetails> findByIds(List<Long> id);

}

package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public interface IELDetailsDao {
    ELDetails save(ELDetails elDetails);

    List<ELDetails> saveAll(List<ELDetails> elDetailsList);

    Optional<ELDetails> findByGuid(UUID guid);

    Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable);

    Optional<ELDetails> findById(Long id);

    void delete(ELDetails elDetails);

    Optional<ELDetails> findByElNumber(String elNumber);

    List<ELDetails> updateEntityFromShipment(List<ELDetails> elDetailsList, Long shipmentId) throws RunnerException;

    List<ELDetails> saveEntityFromShipment(List<ELDetails> elDetails, Long shipmentId);

    List<ELDetails> saveEntityFromShipment(List<ELDetails> elDetails, Long shipmentId, Map<Long, ELDetails> oldEntityMap);

    List<ELDetails> updateEntityFromShipment(List<ELDetails> elDetailsList, Long shipmentId, List<ELDetails> oldEntityList) throws RunnerException;
}

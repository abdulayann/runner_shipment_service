package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface ITruckDriverDetailsDao {
    TruckDriverDetails save(TruckDriverDetails truckDriverDetails);
    Page<TruckDriverDetails> findAll(Specification<TruckDriverDetails> spec, Pageable pageable);
    Optional<TruckDriverDetails> findById(Long id);
    void delete(TruckDriverDetails truckDriverDetails);
    List<TruckDriverDetails> updateEntityFromConsole(List<TruckDriverDetails> truckDriverDetailsList, Long consolidationId) throws Exception;
    List<TruckDriverDetails> updateEntityFromConsole(List<TruckDriverDetails> truckDriverDetailsList, Long consolidationId, List<TruckDriverDetails> oldEntityList) throws Exception;
    List<TruckDriverDetails> saveEntityFromConsole(List<TruckDriverDetails> truckDriverDetailsRequests, Long consolidationId);
    List<TruckDriverDetails> updateEntityFromShipment(List<TruckDriverDetails> truckDriverDetails, Long shipmentId) throws Exception;
    List<TruckDriverDetails> saveEntityFromShipment(List<TruckDriverDetails> truckDriverDetails, Long shipmentId);
    List<TruckDriverDetails> updateEntityFromShipment(List<TruckDriverDetails> truckDriverDetailsList, Long shipmentId, List<TruckDriverDetails> oldEntityList) throws Exception;
}

package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsListResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IPickupDeliveryDetailsDao {
    PickupDeliveryDetails save(PickupDeliveryDetails pickupDeliveryDetails);
    Page<PickupDeliveryDetails> findAll(Specification<PickupDeliveryDetails> spec, Pageable pageable);
    Optional<PickupDeliveryDetails> findById(Long id);
    void delete(PickupDeliveryDetails pickupDeliveryDetails);
    List<PickupDeliveryDetails> updateEntityFromShipment(List<PickupDeliveryDetails> pickupDeliveryDetailsList, Long shipmentId) throws RunnerException;
    List<PickupDeliveryDetails> saveEntityFromShipment(List<PickupDeliveryDetails> pickupDeliveryDetailsRequests, Long shipmentId);
    List<PickupDeliveryDetailsListResponse> findByIdIn(List<Long> ids);
}

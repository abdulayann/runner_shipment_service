package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface IPickupDeliveryDetailsDao {
    PickupDeliveryDetails save(PickupDeliveryDetails pickupDeliveryDetails);
    Page<PickupDeliveryDetails> findAll(Specification<PickupDeliveryDetails> spec, Pageable pageable);
    Optional<PickupDeliveryDetails> findById(Long id);
    void delete(PickupDeliveryDetails pickupDeliveryDetails);
    List<PickupDeliveryDetails> updateEntityFromShipment(List<PickupDeliveryDetails> pickupDeliveryDetailsList, Long shipmentId) throws RunnerException;
    List<PickupDeliveryDetails> saveEntityFromShipment(List<PickupDeliveryDetails> pickupDeliveryDetailsRequests, Long shipmentId);
    List<PickupDeliveryDetails> findByIdIn(List<Long> ids);
    List<PickupDeliveryDetails> findByShipmentId(Long shipmentId);

    Long getTotalTransportInstructionCountIncludeDeleted(Long shipmentId);
    List<PickupDeliveryDetails> findByShipmentIdIn(Set<Long> shipmentIds);

    void deleteAdditionalPickupDeliveryDetailsByShipmentId(List<Long> pickupDeliveryDetailsIds, Long shipmentId);

    void revertSoftDeleteByPickupDeliveryDetailsIdsAndShipmentId(List<Long> pickupDeliveryDetailsIds, Long shipmentId);
}

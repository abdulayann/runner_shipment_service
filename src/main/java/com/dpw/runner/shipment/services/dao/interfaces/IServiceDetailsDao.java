package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ServiceDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IServiceDetailsDao {
    ServiceDetails save(ServiceDetails serviceDetails);
    Page<ServiceDetails> findAll(Specification<ServiceDetails> spec, Pageable pageable);
    Optional<ServiceDetails> findById(Long id);
    void delete(ServiceDetails serviceDetails);
    List<ServiceDetails> updateEntityFromShipment(List<ServiceDetails> serviceDetailsList, Long shipmentId) throws Exception;
    List<ServiceDetails> saveEntityFromShipment(List<ServiceDetails> serviceDetailsRequests, Long shipmentId);
}

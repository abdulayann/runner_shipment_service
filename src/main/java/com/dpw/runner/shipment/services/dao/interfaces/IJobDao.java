package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Jobs;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IJobDao {
    Jobs save(Jobs jobs);
    Page<Jobs> findAll(Specification<Jobs> spec, Pageable pageable);
    Optional<Jobs> findById(Long id);
    void delete(Jobs jobs);
    List<Jobs> updateEntityFromShipment(List<Jobs>jobsList, Long shipmentId) throws Exception;
    List<Jobs> saveEntityFromShipment(List<Jobs> jobRequests, Long shipmentId);
    List<Jobs> updateEntityFromConsole(List<Jobs>jobsList, Long shipmentId) throws Exception;
    List<Jobs> updateEntityFromConsole(List<Jobs>jobsList, Long shipmentId, List<Jobs> oldEntityList) throws Exception;
    List<Jobs> saveEntityFromConsole(List<Jobs> jobRequests, Long shipmentId);
    List<Jobs> updateEntityFromShipment(List<Jobs>jobsList, Long shipmentId, List<Jobs> oldEntityList) throws Exception;
}

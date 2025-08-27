package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.response.consolidation.IContainerLiteResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.projection.ContainerDeleteInfoProjection;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

public interface IContainerDao {
    Containers save(Containers containers);

    Page<Containers> findAll(Specification<Containers> spec, Pageable pageable);

    Page<Containers> findAllWithoutTenantFilter(Specification<Containers> spec, Pageable pageable);

    List<IContainerLiteResponse> findAllLiteContainer(List<Long> consolidationId);

    List<Containers> getAllContainers();

    Optional<Containers> findById(Long id);
    List<Containers> findByGuid(UUID guid);

    void delete(Containers containers);

    List<Containers> updateEntityFromShipmentConsole(List<Containers> containersList, Long consolidationId, Long shipmentId, boolean fromConsolidation) throws RunnerException;

    List<Containers> updateEntityFromBooking(List<Containers> containersList, Long bookingId) throws RunnerException;

    List<Containers> saveAll(List<Containers> containers);

    List<Containers> updateEntityFromConsolidationV1(List<Containers> containersList, Long consolidationId, List<Containers> oldContainers) throws RunnerException;
    List<Containers> updateEntityFromShipmentV1(List<Containers> containersList, List<Containers> oldContainers) throws RunnerException;
    List<Containers> findByShipmentId(Long shipmentId);

    List<Containers> findByShipmentIdWithoutTenantFilter(Long shipmentId);

    List<Containers> findByConsolidationIdWithoutTenantFilter(Long consolidationId);

    List<Containers> findByConsolidationId(Long consolidationId);
    List<Containers> findByConsolidationIdIn(List<Long> consolidationIds);
    List<Containers> findByBookingIdIn(List<Long> bookingIds);
    void deleteAllById(List<Long> containerIdList);
    void deleteById(Long id);
    List<Containers> findByIdIn(List<Long> containerIds);

    void deleteByIdIn(List<Long> deleteContainerIds);

    List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToShipmentCargo(List<Long> containerIds);

    List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToShipment(List<Long> containerIds);

    List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToPacking(List<Long> containerIds);

    List<ContainerDeleteInfoProjection> findContainersAttachedToBothPackingAndCargo(List<Long> containerIds);

    List<Long> findContainerIdsAttachedToEitherPackingOrShipment(List<Long> containerIds);

    List<ContainerInfoProjection> findByContainerIds(List<Long> containerIds);

    void deleteAdditionalDataByContainersIdsConsolidationId(List<Long> containersIds, Long consolidationId);

    void revertSoftDeleteByContainersIdsAndConsolidationId(List<Long> containersIds, Long consolidationId);

    void deleteAdditionalDataByContainersIdsBookingId(List<Long> containersIds, Long bookingId);

    void revertSoftDeleteByContainersIdsAndBookingId(List<Long> containersIds, Long bookingId);

    void deleteAll(List<Containers> containers);
}

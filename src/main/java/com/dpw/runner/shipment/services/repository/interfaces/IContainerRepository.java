package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.response.consolidation.IContainerLiteResponse;
import com.dpw.runner.shipment.services.projection.ContainerDeleteInfoProjection;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

@Generated
public interface IContainerRepository extends MultiTenancyRepository<Containers> {

    List<Containers> findAll();

    Page<Containers> findAll(Specification<Containers> spec, Pageable pageable);

    @ExcludeTenantFilter
    default Page<Containers> findAllWithoutTenantFilter(Specification<Containers> spec, Pageable pageable){
        return findAll(spec, pageable);
    }

    List<Containers> findByGuid(UUID guid);

    default Optional<Containers> findById(Long id) {
        Specification<Containers> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }

    List<Containers> findByConsolidationId(Long consolidationId);

    @ExcludeTenantFilter
    default List<Containers> findByConsolidationIdWithoutTenantFilter(Long consolidationId){
        return findByConsolidationId(consolidationId);
    }

    void deleteById(Long id);

    @Modifying
    @Transactional
    @ExcludeTenantFilter
    @Query(value = "Update containers set pra_status = ?1 where guid = ?2 and consolidation_id = ?3", nativeQuery = true)
    void savePraStatus(String praStatus, UUID guid, Long consolidationId);

    List<Containers> findByConsolidationIdIn(List<Long> consolidationIds);

    List<Containers> findByBookingIdIn(List<Long> bookingIds);

    List<Containers> findByIdIn(List<Long> containerIds);

    @Query(value = "SELECT c.consolidation_id AS consolidationId, c.container_code AS containerCode, c.container_number AS containerNumber, c.container_count AS containerCount "
            + "FROM containers c WHERE c.consolidation_id IN :consolidationIds", nativeQuery = true)
    List<IContainerLiteResponse> findAllLiteContainer(@Param("consolidationIds") List<Long> consolidationId);

    // Retrieves a list of unique container IDs that are assigned to shipments and are not marked as deleted.
    @Query(value = """
            SELECT DISTINCT
                 sd.shipment_id AS shipmentId,
                 c.container_code AS containerCode,
                 c.container_number AS containerNumber,
                 c.id AS containerId
             FROM
                 containers c
             JOIN
                 shipment_details sd
                 ON sd.container_assigned_to_shipment_cargo = c.id
             WHERE
                 c.id IN (?1)
                 AND c.is_deleted = false
                 AND sd.is_deleted = false
            """, nativeQuery = true)
    List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToShipmentCargo(List<Long> containerIds);

    // Retrieves a list of unique container IDs that are associated with packings,
    // ensuring the containers and packings are not marked as deleted.
    @Query(value = """
                SELECT c.id as containerId,
                c.container_number AS containerNumber,
                c.container_code AS containerCode,
                sd.shipment_id AS shipmentId,
                p.packs AS packs,
                p.packs_type AS packsType
                FROM containers c
                inner join packing p on c.id = p.container_id
                inner join shipment_details sd on sd.id = p.shipment_id
                WHERE c.id IN (?1)
                  AND c.is_deleted = false
                  AND p.is_deleted = false
                  AND sd.is_deleted = false
            """, nativeQuery = true)
    List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToPacking(List<Long> containerIds);

    @Query(value = """
                SELECT
                    sd.shipment_id AS shipmentId,
                c.container_number AS containerNumber,
                c.container_code AS containerCode,
                    scm.container_id AS containerId,
                    p.packs AS packs,
                    p.packs_type AS packsType
                FROM shipments_containers_mapping scm
                INNER JOIN containers c ON c.id = scm.container_id
                INNER JOIN shipment_details sd ON scm.shipment_id = sd.id
                LEFT JOIN packing p ON p.container_id = c.id AND p.is_deleted = false
                WHERE c.id IN (?1)
                  AND c.is_deleted = false
                  AND sd.is_deleted = false
            """, nativeQuery = true)
    List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToShipment(List<Long> containerIds);

    @Query(value = """
            SELECT DISTINCT c.id AS containerId,
                   c.container_number AS containerNumber,
                   c.container_code AS containerCode,
                   sd.shipment_id AS shipmentId,
                   p.packs_type AS packsType
                   p.packs AS packs
            FROM containers c
            INNER JOIN shipment_details sd ON sd.container_assigned_to_shipment_cargo = c.id
            INNER JOIN packing p ON p.container_id = c.id AND p.shipment_id = sd.id
            WHERE c.id IN (?1)
              AND c.is_deleted = false
              AND sd.is_deleted = false
              AND p.is_deleted = false
            """, nativeQuery = true)
    List<ContainerDeleteInfoProjection> findContainersAttachedToBothPackingAndCargo(List<Long> containerIds);

    @Query(value = """
            SELECT DISTINCT c.id
            FROM containers c
            LEFT JOIN shipments_containers_mapping scm ON scm.container_id = c.id AND scm.is_deleted = false
            LEFT JOIN packing p ON p.container_id = c.id AND p.is_deleted = false
            WHERE c.is_deleted = false
              AND c.id IN (?1)
              AND (scm.id IS NOT NULL OR p.id IS NOT NULL)
            """, nativeQuery = true)
    List<Long> findContainerIdsAttachedToEitherPackingOrShipment(List<Long> containerIds);

    @Query(value = """
            SELECT c.id as id, c.containerNumber as containerNumber, c.containerCode as containerCode
            FROM Containers c
            WHERE c.id in(:containerIds)
            """)
    List<ContainerInfoProjection> findByContainerIds(@Param("containerIds") List<Long> containerIds);

    @Modifying
    @Query(value = "UPDATE containers SET is_deleted = true WHERE id NOT IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void deleteAdditionalDataByContainersIdsConsolidationId(List<Long> containersIds, Long consolidationId);

    @Modifying
    @Query(value = "UPDATE containers SET is_deleted = false WHERE id IN (?1) and consolidation_id = ?2", nativeQuery = true)
    void revertSoftDeleteByContainersIdsAndConsolidationId(List<Long> containersIds, Long consolidationId);
}

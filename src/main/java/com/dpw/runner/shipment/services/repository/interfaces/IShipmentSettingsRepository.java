package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Generated
public interface IShipmentSettingsRepository extends MultiTenancyRepository<ShipmentSettingsDetails> {
    Page<ShipmentSettingsDetails> findAll(Specification<ShipmentSettingsDetails> spec, Pageable pageable);
    Optional<ShipmentSettingsDetails> findByGuid(UUID guid);
    default Optional<ShipmentSettingsDetails> findById(Long id) {
        Specification<ShipmentSettingsDetails> spec = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), id);
        return findOne(spec);
    }
    List<ShipmentSettingsDetails> findAll();
    @Query(value = "SELECT sd.shipment_console_import_approver_role FROM shipment_setting sd WHERE sd.tenant_id = ?1", nativeQuery = true)
    Integer getShipmentConsoleImportApprovarRole(int tenantId);

    @Query(value = "SELECT * FROM shipment_setting sd WHERE sd.tenant_id IN ?1 AND sd.is_deleted = false", nativeQuery = true)
    List<ShipmentSettingsDetails> getTenantSetting(List<Integer> tenantId);
    Optional<ShipmentSettingsDetails> findByTenantId(Integer tenantId);

    @Query(value = "SELECT customised_sequence FROM shipment_setting sd where sd.tenant_id = ?1")
    Boolean getCustomisedSequence(Integer tenantId);
}

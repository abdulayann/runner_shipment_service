package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.utils.ExcludeTenantFilter;
import com.dpw.runner.shipment.services.utils.Generated;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

@Generated
public interface ICarrierRepository extends JpaRepository<CarrierDetails, Long> {
    @Modifying @Transactional
    @Query(value = "Update carrier_details set updated_at = now(), origin_loc_code = ?2, origin_port_loc_code = ?3, " +
            "destination_loc_code = ?4, destination_port_loc_code = ?5 Where id = ?1", nativeQuery = true)
    void saveUnLocCodes(Long id, String originLoc, String originPortLoc, String destinationLoc, String destinationPortLoc);

    List<CarrierDetails> findByIdIn(List<Long> ids);

    @Modifying
    @Transactional
    @ExcludeTenantFilter
    @Query(value = "UPDATE carrier_details " +
            "SET updated_at = now(), ata = ?2 " +
            "WHERE id = ?1", nativeQuery = true)
    void updateAta(Long id, LocalDateTime shipmentAta);

    @Modifying
    @Transactional
    @ExcludeTenantFilter
    @Query(value = "UPDATE carrier_details " +
            "SET updated_at = now(), atd = ?2 " +
            "WHERE id = ?1", nativeQuery = true)
    void updateAtd(Long id, LocalDateTime shipmentAta);
}

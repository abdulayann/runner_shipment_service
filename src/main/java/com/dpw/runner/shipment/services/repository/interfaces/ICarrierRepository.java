package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.utils.Generated;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

@Generated
public interface ICarrierRepository extends JpaRepository<CarrierDetails, Long> {
    @Modifying @Transactional
    @Query(value = "Update carrier_details set origin_loc_code = ?2, origin_port_loc_code = ?3, " +
            "destination_loc_code = ?4, destination_port_loc_code = ?5 Where id = ?1", nativeQuery = true)
    void saveUnLocCodes(Long id, String originLoc, String originPortLoc, String destinationLoc, String destinationPortLoc);

    List<CarrierDetails> findByIdIn(List<Long> ids);
}

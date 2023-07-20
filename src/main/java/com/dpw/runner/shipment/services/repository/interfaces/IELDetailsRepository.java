package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.entity.ELDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static com.dpw.runner.shipment.services.commons.constants.DBQueryConstants.ELDETAILS_SELECT_ELNUMBERS_QUERY;

public interface IELDetailsRepository extends JpaRepository<ELDetails, Long> {
    Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable);

    @Query(ELDETAILS_SELECT_ELNUMBERS_QUERY)
    Optional<ELDetails> findByElNumber(String elNumber);

    List<ELDetails> findByShipmentId(Long shipmentId);
    Optional<ELDetails> findByGuid(UUID guid);

}

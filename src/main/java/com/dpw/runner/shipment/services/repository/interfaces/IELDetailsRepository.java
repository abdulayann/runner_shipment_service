package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.ELDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.commons.constants.DBQueryConstants.ELDETAILS_SELECT_ELNUMBERS_QUERY;

public interface IELDetailsRepository extends MultiTenancyRepository<ELDetails> {
    Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable);
    Optional<ELDetails> findByElNumber(String elNumber);

    List<ELDetails> findByShipmentId(Long shipmentId);
    Optional<ELDetails> findById(Long id);
    List<ELDetails> findAll();

}

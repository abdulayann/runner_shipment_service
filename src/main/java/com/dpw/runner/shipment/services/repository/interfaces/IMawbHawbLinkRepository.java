package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IMawbHawbLinkRepository extends JpaRepository<MawbHawbLink, Long> {
    Page<MawbHawbLink> findAll(Specification<MawbHawbLink> spec, Pageable pageable);
}

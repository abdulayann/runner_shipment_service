package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IVerifiedGrossMassDao {

    Optional<VerifiedGrossMass> findById(Long id);

    Page<VerifiedGrossMass> findAll(Specification<VerifiedGrossMass> spec, Pageable pageable);

    VerifiedGrossMass update(Long id, VerifiedGrossMass verifiedGrossMass);

    void delete(Long id);

    VerifiedGrossMass save(VerifiedGrossMass verifiedGrossMass);
}



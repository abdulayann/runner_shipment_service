package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IMawbHawbLinkDao {
    MawbHawbLink save(MawbHawbLink mawbHawbLink);

    Page<MawbHawbLink> findAll(Specification<MawbHawbLink> spec, Pageable pageable);

    Optional<MawbHawbLink> findById(Long id);

    void delete(MawbHawbLink mawbHawbLink);

    List<MawbHawbLink> findByMawbId(Long mawbId);

    List<MawbHawbLink> findByHawbId(Long hawbId);
}

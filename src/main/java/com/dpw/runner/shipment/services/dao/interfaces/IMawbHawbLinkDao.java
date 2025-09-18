package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface IMawbHawbLinkDao {
    MawbHawbLink save(MawbHawbLink mawbHawbLink);
    List<MawbHawbLink> saveAll(List<MawbHawbLink> mawbHawbLinks);
    Page<MawbHawbLink> findAll(Specification<MawbHawbLink> spec, Pageable pageable);
    Optional<MawbHawbLink> findById(Long id);
    void delete(MawbHawbLink mawbHawbLink);
    void deleteByHawbIdsAndMawbIds(Set<Long> hawbIds, Long mawbId);
    List<MawbHawbLink> findByMawbId(Long mawbId);
    List<MawbHawbLink> findByHawbId(Long hawbId);
}

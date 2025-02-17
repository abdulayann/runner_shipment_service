package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Generated
public interface IMawbStocksLinkRepository extends MultiTenancyRepository<MawbStocksLink> {
    Page<MawbStocksLink> findAll(Specification<MawbStocksLink> spec, Pageable pageable);

    List<MawbStocksLink> findByMawbNumber(String mawbNumber);

    @Modifying
    @Query(nativeQuery = true, value = "delete from mawb_stocks_link where parent_id = ?1")
    void deleteByParentId(Long parentId);
}

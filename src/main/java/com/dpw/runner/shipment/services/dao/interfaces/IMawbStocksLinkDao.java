package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface IMawbStocksLinkDao {
    MawbStocksLink save(MawbStocksLink mawbStocksLink);

    Page<MawbStocksLink> findAll(Specification<MawbStocksLink> spec, Pageable pageable);

    List<MawbStocksLink> findByMawbNumber(String mawbNumber);

    void deleteByParentId(Long parentId);

    void deLinkExistingMawbStockLink(String mawbNumber);

    String assignNextMawbNumber(Long parentId);

    Long validateDuplicateMawbNumber(List<String> mawbNumbers);
}

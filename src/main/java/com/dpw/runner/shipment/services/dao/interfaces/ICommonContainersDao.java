package com.dpw.runner.shipment.services.dao.interfaces;


import com.dpw.runner.shipment.services.entity.CommonContainers;

import java.util.List;
import java.util.UUID;

public interface ICommonContainersDao  {
    CommonContainers getByGuid(String guid);

    void saveAll(List<CommonContainers> commonContainersList);

    List<CommonContainers> getAll(List<UUID> guids);
}

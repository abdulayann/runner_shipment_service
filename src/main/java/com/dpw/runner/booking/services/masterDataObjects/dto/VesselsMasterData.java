package com.dpw.runner.booking.services.masterDataObjects.dto;


import com.dpw.runner.booking.services.masterDataObjects.common.request.IMasterDataBaseEntity;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class VesselsMasterData implements IMasterDataBaseEntity, Serializable {
    public UUID Guid;
    public String Imo;
    public String Mmsi;
    public String Name;
    public String Flag;
    public String Type;
}
